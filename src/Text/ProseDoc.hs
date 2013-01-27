{-%
# Text.ProseDoc

`Text.ProseDoc` is a tool that reads markdown formatted comments from a
Haskell source file and composes the comments and the associated source
into a HTML document where the prose and source flow side by side in sync.

It can be seen as an alternative way to write literal Haskell code.
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.ProseDoc where

import Control.Applicative (Applicative, (<$>))
import Control.Error
import Control.Monad.State
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.List (tails, intercalate, sortBy, span, sort)
import Data.List.Split
import Data.String (fromString)
import Data.Ord (comparing)
import Data.Char (isSpace)
import qualified Data.Text as T

import Text.Blaze.Html5 ((!))
import Text.Blaze.Extra
import Text.Blaze.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.SelfContained
import Text.Pandoc.Parsing (defaultParserState)
import Text.Pandoc.Shared (defaultWriterOptions)

{-%
## Lexing Haskell Source Code

We use the [`haskell-src-exts`](http://hackage.haskell.org/package/haskell-src-exts)
package to lex the Haskell code. This lets us correctly parse and syntax highlight
almost all the syntactic extensions supported by modern GHC.
-}
import Language.Haskell.Exts (readExtensions, parseFileContentsWithComments)
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Annotated.Syntax as S

import Text.ProseDoc.Tree
import Text.ProseDoc.Tree.Builder
import Text.ProseDoc.Classifier
import Text.ProseDoc.Classifier.Types
import Text.ProseDoc.Classifier.Tokens

testClassifier :: IO ()
testClassifier = runScript $ do
    tree <- parseSourceFile "src/Text/ProseDoc.hs"
    let sections = extractSections tree
    -- scriptIO $ print tree
    -- scriptIO $ print sections

    scriptIO
        $ writeFile "ProseDoc.html"
        <=< makeSelfContained (Just "css")
        . renderHtml
        . renderPage
        $ sections

parseSourceFile :: FilePath -> EitherT String IO (Tree Classifier Printable)
parseSourceFile fp = do
    src  <- scriptIO $ readFile fp
    hoistEither $ buildTree fp src

buildTree :: FilePath -> String -> Either String (Tree Classifier Printable)
buildTree path src = case parseResult . parseMode <$> readExtensions src of
    Nothing                    -> Left "unable to parse language extensions"
    Just (ParseOk r)           -> return r
    Just (ParseFailed loc msg) -> Left (prettyPrint loc ++ ": " ++ msg)

    where
        parseResult mode = do
            tokens       <- lexTokenStreamWithMode mode src
            (ast :: S.Module SrcSpan, comments) <- parseWithComments mode src

            let builder   = mkTree ast
                tree      = runTreeBuilder builder src fragments
                fragments
                    =  sort
                    $  concatMap toFragments tokens
                    ++ concatMap toFragments comments

            return tree

        parseMode exts = defaultParseMode
            { parseFilename = path
            , fixities = Just []
            , extensions = exts
            }

extractSections :: Tree Classifier Printable -> [Section]
extractSections = map toSection . splitTree isProse where
    toSection (sep, tree) = case sep of
        Nothing -> Section "" tree
        Just (Label (ProseComment prose) _) -> Section prose tree

sectionToHtml :: Section -> H.Html
sectionToHtml (Section {..}) = H.tr (proseTd <> codeTd) where
    proseTd = H.td !. "prose" $ markdownToHtml sectionProse

    codeTd  = H.td !. "code"
        $ H.pre
        $ H.code !. "haskell"
        $ treeToHtml sectionCode

markdownToHtml :: String -> H.Html
markdownToHtml = writeHtml defaultWriterOptions . readMarkdown defaultParserState

treeToHtml :: Tree Classifier Printable -> H.Html
treeToHtml = foldTree addSpan H.toHtml where
    addSpan cls inner = case cssClass cls of
        Nothing -> inner
        Just c  -> H.span !. c $ inner

    cssClass Keyword = Just "kw"
    cssClass Pragma  = Just "kw"
    cssClass ModulePragma = Just "pragma"
    cssClass ModuleName = Just "module-name"
    cssClass Name = Just "name"
    cssClass Signature = Just "typesig"
    cssClass TypeName = Just "type-name"
    cssClass _       = Nothing

renderPage :: [Section] -> H.Html
renderPage sections = H.docTypeHtml $ head >> body where
    head = H.head $ css >> H.title "ProseDoc Generated Module Listing"
    body = H.body $ (H.table !. "sections") $ mapM_ sectionToHtml sections

    css = H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "prose.css"

data Section = Section
    { sectionProse :: String
    , sectionCode  :: Tree Classifier Printable
    } deriving Show
