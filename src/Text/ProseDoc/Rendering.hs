{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ProseDoc.Rendering where

import Control.Monad.State
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))

import Data.Monoid
import Data.String (fromString)
import Data.List   (isPrefixOf, stripPrefix, intercalate)

import Text.Blaze.Html5 ((!))
import Text.Blaze.Extra
import Text.Blaze.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Parsing (defaultParserState)
import Text.Pandoc.Shared (defaultWriterOptions)

import Text.ProseDoc.Tree
import Text.ProseDoc.Classifier.Types

import System.FilePath

htmlTOC :: [FilePath] -> H.Html
htmlTOC = (H.ul !. "toc") . evalState (go []) . idify  where

    go prefix = do
        paths <- get
        case paths of
            []       -> return mempty
            ((fp,anchor):fps)  ->
                let parts  = splitDirectories fp
                    rest   = stripPrefix prefix parts
                in case rest of
                    Nothing -> return mempty
                    Just r  -> put fps >> go2 prefix (r,anchor)

    go2 prefix ((x:xs), anchor) = do
        let base  = takeBaseName x
            label = H.toHtml (intercalate "." (prefix ++ [base]))
            link  = H.a ! A.href (fromString ('#' : anchor)) $ label
            tag   = if null xs then link else label
        descend <- case xs of
            [] -> go (prefix ++ [base])
            _  -> go2 (prefix ++ [x]) (xs, anchor)
        siblings <- go prefix
        return $ H.li tag <> H.ul descend <> siblings

    idify = map (id &&& pathToId)

moduleToHtml :: (FilePath, Tree Classifier Printable) -> H.Html
moduleToHtml (fp, t)
    =  H.tr !. "file-header" $ (H.td anchor <> fileTd )
    <> treeToHtml t
    where
        fileTd = H.td . H.code . H.toHtml $ fp
        anchor = H.a !# fromString (pathToId fp) $ ""


pathToId :: FilePath -> String
pathToId = map replaceChar where
    replaceChar c
        | c `elem` "./" = '-'
        | otherwise     = c

extractSections :: Tree Classifier Printable -> [Section]
extractSections = (pad:) . (++[pad]) . map toSection . splitTree isProse where
    toSection (sep, tree) = case sep of
        Nothing -> Section "" tree
        Just (Label (ProseComment prose) _) -> Section prose tree
    pad = Section "" (Leaf "\n")

treeToHtml :: Tree Classifier Printable -> H.Html
treeToHtml = mapM_ sectionToHtml . extractSections

sectionToHtml :: Section -> H.Html
sectionToHtml (Section {..}) = H.tr (proseTd <> codeTd) where
    proseTd = H.td !. "prose" $ markdownToHtml sectionProse

    codeTd  = H.td !. "code"
        $ H.pre
        $ H.code !. "haskell"
        $ codeTreeToHtml sectionCode

markdownToHtml :: String -> H.Html
markdownToHtml
    = writeHtml defaultWriterOptions
    . readMarkdown defaultParserState

codeTreeToHtml :: Tree Classifier Printable -> H.Html
codeTreeToHtml = foldTree addSpan H.toHtml . pruneEmptyBranches where
    addSpan cls inner = case unwords (cssClass cls) of
        "" -> inner
        c  -> H.span !. fromString c $ inner

    cssClass Keyword         = ["kw"]
    cssClass Pragma          = ["kw"]
    cssClass ModulePragma    = ["pragma"]
    cssClass ModuleName      = ["module-name"]
    cssClass Name            = ["name"]
    cssClass Signature       = ["typesig"]
    cssClass TypeName        = ["type-name"]
    cssClass ConstrName      = ["constr-name"]
    cssClass Braces          = ["brace"]
    cssClass SpecPunctuation = ["syntax"]
    cssClass Punctuation     = ["punct"]
    cssClass InfixOperator   = ["infix-op"]
    cssClass StringLit       = ["lit", "string"]
    cssClass LineComment     = ["comment"]
    cssClass BlockComment    = ["comment"]
    cssClass ImportDecl      = ["import"]
    cssClass _               = []

renderPage :: H.Html -> [H.Html] -> String
renderPage toc mods = renderHtml . H.docTypeHtml $ docHead >> docBody where
    docHead = H.head $ css >> H.title "ProseDoc Generated Module Listing"
    docBody = H.body $ (H.table !. "sections") $ toc <> mconcat mods

    css = H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "prose.css"

data Section = Section
    { sectionProse :: String
    , sectionCode  :: Tree Classifier Printable
    } deriving Show
