{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ProseDoc.Rendering where

import Data.Monoid ((<>))
import Data.String (fromString)

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
markdownToHtml
    = writeHtml defaultWriterOptions
    . readMarkdown defaultParserState

treeToHtml :: Tree Classifier Printable -> H.Html
treeToHtml = foldTree addSpan H.toHtml where
    addSpan cls inner = case unwords (cssClass cls) of
        "" -> inner
        c  -> H.span !. fromString c $ inner

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

renderPage :: [Section] -> String
renderPage sections = renderHtml . H.docTypeHtml $ docHead >> docBody where
    docHead = H.head $ css >> H.title "ProseDoc Generated Module Listing"
    docBody = H.body $ (H.table !. "sections") $ mapM_ sectionToHtml sections

    css = H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "prose.css"

data Section = Section
    { sectionProse :: String
    , sectionCode  :: Tree Classifier Printable
    } deriving Show
