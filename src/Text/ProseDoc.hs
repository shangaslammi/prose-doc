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
import qualified Language.Haskell.Exts.Annotated.Syntax as S

import Text.ProseDoc.Tree
import Text.ProseDoc.Tree.Builder
import Text.ProseDoc.Classifier
import Text.ProseDoc.Classifier.Types
import Text.ProseDoc.Classifier.Tokens

testClassifier :: IO ()
testClassifier = runScript $ do
    src <- scriptIO $ readFile "src/Text/ProseDoc.hs"
    tree <- hoistEither $ lexHaskell' "src/Text/ProseDoc.hs" src

    scriptIO $ print tree


lexHaskell' :: FilePath -> String -> Either String (Tree Classifier Printable)
lexHaskell' path src = case parseResult . parseMode <$> readExtensions src of
    Nothing   -> Left "unable to parse language extensions"
    Just (ParseOk r) -> return r
    Just (ParseFailed loc msg) -> Left msg

    where
        parseResult mode = do
            tokens       <- lexTokenStreamWithMode mode src
            (ast :: S.Module SrcSpan, comments) <- parseWithComments mode src

            let builder   = mkTree ast
                tree      = runTreeBuilder builder src (sort fragments)
                fragments
                    =  concatMap toFragments tokens
                    ++ concatMap toFragments comments

            return tree

        parseMode exts = defaultParseMode
            { parseFilename = path
            , fixities = Just []
            , extensions = exts
            }

lexHaskell :: FilePath -> String -> Either String [Loc Token']
lexHaskell path src = case parseResult of
    ParseOk r -> return r
    ParseFailed loc msg -> Left msg

    where
        parseResult = do
            tokens       <- lexTokenStreamWithMode parseMode src
            (_,comments) <- parseFileContentsWithComments parseMode src
            return $ interleaveComments tokens comments

        parseMode = defaultParseMode
            { parseFilename = path
            , ignoreLanguagePragmas = False
            , fixities = Just []
            }

{-%
For error handling the application runs in a `EitherT String IO` monad stack
which is very convenient with the [`errors`](http://hackage.haskell.org/package/errors)
package.
-}
type ProseDoc a = EitherT String IO a

{-%
The `Tagged` type allows as a tag any type with textual data. We use it to
attach each `Token` with the associated piece of source code.
-}
data Tagged a = Tagged { tag :: String, unTag :: a } deriving Show

tagTokens :: [String] -> [Loc a] -> [Tagged a]
tagTokens srcLines = flip evalState (1,1) . mapM go where
    go (Loc s@(SrcSpan{..}) a) = do
        (prevLine, prevCol) <- get

        let deltaLines = srcSpanStartLine - prevLine
            deltaCol
                | deltaLines == 0 = srcSpanStartColumn - prevCol
                | otherwise       = srcSpanStartColumn - 1

            ws = replicate deltaLines '\n' ++ replicate deltaCol ' '
            tokenSrc = extractSpan

        put (srcSpanEndLine, srcSpanEndColumn)
        return $ Tagged (ws ++ extractSpan s srcLines) a

{-%
It would be incredibly useful if `Token` contained the original string that
the `Token` was created from, but alas we have to use the `SrcSpan` to dig
into the source code and retrieve the contents of the span.

The `extractSpan` function is highly inefficient as it naively traverses
the list of source lines over and over, but it's probably not a problem
in practice for the typical amount of lines in a single file.
-}
extractSpan :: SrcSpan -> [String] -> String
extractSpan (SrcSpan{..}) = intercalate "\n" . go . zip [1..] where
    go ((n, l):lns)
        | n <  srcSpanStartLine = go lns
        | n == srcSpanStartLine = if srcSpanStartLine == srcSpanEndLine
            then [take len $ drop (srcSpanStartColumn-1) l]
            else drop (srcSpanStartColumn-1) l : go lns
        | n <  srcSpanEndLine   = l : go lns
        | n == srcSpanEndLine   = [take (srcSpanEndColumn) l]
        | otherwise             = []
        where len = srcSpanEndColumn - srcSpanStartColumn

{-%
The source file is partitioned into sections, breaking at every block comment
that begins with the character '%'
-}
data Section = Section String [Tagged Token'] deriving Show

splitSections :: [Tagged Token'] -> [Section]
splitSections = map toSections . split rules where

    rules = dropFinalBlank . dropInitBlank . keepDelimsL $ whenElt isProseToken

    isProseToken (proseContent -> Just _) = True
    isProseToken _ = False

    proseContent (unTag -> (Comment' (Comment True _ ('%':txt)))) = Just txt
    proseContent _ = Nothing

    toSections tokens@(t:ts) = case proseContent t of
        Just s  -> Section s $ case ts of
            (x:xs) -> fixEOL x : xs
            _      -> ts
        Nothing -> Section "" tokens

    fixEOL (Tagged ('\n':s) t) = Tagged s t

{-%
We read the source from file, apply the lexer from [`Language.Haskell.Exts.Lexer`](http://hackage.haskell.org/packages/archive/haskell-src-exts/1.13.5/doc/html/Language-Haskell-Exts-Lexer.html)
-}
parseSourceFile :: FilePath -> ProseDoc [Section]
parseSourceFile fp = do
    src <- scriptIO $ readFile fp
    hoistEither $ (splitSections . tagTokens (lines src)) <$> lexHaskell fp src

{-%
## Lexing Comments

There's an unfortunate shortcoming (for our particular use-case) in
`Language.Haskell.Exts.Lexer` that there are no tokens for comments. The package
does contain parse functions that also return comments with source locations,
but AST is not as practical for this kind of source formatting compared to a
plain `Token` list.

Moreover, since we now need to process data of two different types (`Token` and
`Comment`), we introduce an ADT that encompasses both.
-}
data Token'
    = Token' Token
    | Comment' Comment
    deriving Show

instance Functor Loc where
    fmap f (Loc l a) = Loc l (f a)

{-%
The comments are parsed separately, but fortunately we can use the location
information in the comments' `SrcSpan` to interleave them in their original
positions in the `Token` list.
-}
interleaveComments :: [Loc Token] -> [Comment] -> [Loc Token']
interleaveComments tokens comments = sortByLoc combinedList where
    sortByLoc    = sortBy $ comparing loc
    combinedList = map fromToken tokens ++ map fromComment comments
    fromToken    = fmap Token'
    fromComment c@(Comment _ s _) = Loc s (Comment' c)


{-%
## Syntax Highlighting

We format the tokens into HTML using the package [`blaze-html`](http://hackage.haskell.org/package/blaze-html).
The whole thing is enclosed in a `<code>` block and highlighted tokens are
wrapped in `<span>` elements with an appropriate CSS class.
-}
syntaxHighlight :: [Tagged Token'] -> H.Html
syntaxHighlight = H.pre . (H.code `cls` "haskell") . H.toHtml . map format where

    format token = case syntaxClass (unTag token) of
        Nothing -> H.toHtml $ tag token
        Just c  -> do
            H.toHtml ws
            H.span `cls` c $ H.toHtml content
            where (ws,content) = span isSpace $ tag token

    syntaxClass (Token' t) | t `elem` keywords = Just "kw"
    syntaxClass _ = Nothing

    keywords =
        [ KW_As
        , KW_By
        , KW_Case
        , KW_Class
        , KW_Data
        , KW_Default
        , KW_Deriving
        , KW_Do
        , KW_MDo
        , KW_Else
        , KW_Family
        , KW_Forall
        , KW_Group
        , KW_Hiding
        , KW_If
        , KW_Import
        , KW_In
        , KW_Infix
        , KW_InfixL
        , KW_InfixR
        , KW_Instance
        , KW_Let
        , KW_Module
        , KW_NewType
        , KW_Of
        , KW_Proc
        , KW_Rec
        , KW_Then
        , KW_Type
        , KW_Using
        , KW_Where
        , KW_Qualified
        , KW_Foreign
        , KW_Export
        , KW_Safe
        , KW_Unsafe
        , KW_Threadsafe
        , KW_StdCall
        , KW_CCall
        , KW_CPlusPlus
        , KW_DotNet
        , KW_Jvm
        , KW_Js ]

{-%
## Page Composition

Sections are formatted into a table where the related comment and code are
in adjacent columns on the same row.
-}

formatPage :: [Section] -> H.Html
formatPage sections = H.docTypeHtml $ head >> body where
    head = H.head $ css >> H.title "ProseDoc Generated Module Listing"
    body = H.body $ (H.table `cls` "sections") $ mapM_ toRow sections

    toRow (Section mkdwn tokens) = H.tr (td1 prose >> td2 code) where
        td1   = H.td `cls` "prose"
        td2   = H.td `cls` "code"
        code  = syntaxHighlight tokens
        prose = writeHtml defaultWriterOptions $ readMarkdown defaultParserState mkdwn

    css = H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "prose.css"

selfTest :: IO ()
selfTest = runScript $ do
    sections <- parseSourceFile "Text/ProseDoc.hs"
    scriptIO
        $ writeFile "ProseDoc.html"
        <=< makeSelfContained (Just "css")
        . renderHtml
        . formatPage
        $ sections

cls h c = h ! A.class_ c
