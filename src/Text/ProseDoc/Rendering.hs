{-%
## Rendering to HTML
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ProseDoc.Rendering where

import Control.Monad.State
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))

import Data.Default (def)
import Data.Monoid
import Data.String (fromString)
import Data.List   (isPrefixOf, stripPrefix, intercalate)

import Text.Blaze.Internal (Attributable)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options (ReaderOptions, WriterOptions)

import Text.ProseDoc.Tree
import Text.ProseDoc.Classifier.Types

import System.FilePath

{-%
`htmlTOC` builds a hierarchical &lt;ul&gt; tree from a list of filenames so that
files that are in the same directory are placed as siblings in the tree.
-}
htmlTOC :: [FilePath] -> H.Html
htmlTOC = (H.ul !. "toc") . evalState (go []) . idify  where

{-%
The internal implementation is rather hairy (there has to be a simpler way to
do this!). We maintain the list of remaining filenames in the `State` monad
and pop them out one by one when we are a the appropriate level of the tree.

We use two mutually recursive functions `go` and `go2`.

`go` processes module
names from the stack until it hits a name that doesn't match the current module
prefix.
-}
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

{-%
`go2` inserts the nested levels for a single module name
-}
    go2 prefix ((x:xs), anchor) = do
        let base  = takeBaseName x
            label = H.toHtml (intercalate "." (prefix ++ [base]))
            link  = H.a ! A.href (fromString ('#' : anchor)) $ label
            tag   = if null xs then link else label
        descend <- case xs of
{-%
`go (prefix ++ [base])` handles cases where we've just processed e.g. module
`Foo/Bar.hs` and created a link for it. Next we have to look for possible child
modules under `Foo/Bar/`.
-}
            [] -> go (prefix ++ [base])
{-%
If we still have parts of the current module name left, recurse `go2` with the
next name part.
-}
            _  -> go2 (prefix ++ [x]) (xs, anchor)

{-%
Finally, handle sibling modules that share the same prefix.
-}
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
pathToId = map replaceChar . dropExtension where
    replaceChar c
        | c `elem` pathSeparators = '.'
        | otherwise     = c

data Section = Section
    { sectionProse :: String
    , sectionCode  :: Tree Classifier Printable
    } deriving Show

{-%
Split the presentation tree into sections at every `ProseComment` label.
-}
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
    = writeHtml def
    . readMarkdown def

codeTreeToHtml :: Tree Classifier Printable -> H.Html
codeTreeToHtml = foldTree addSpan H.toHtml . pruneEmptyBranches where
    addSpan cls inner = case unwords (cssClass cls) of
        "" -> inner
        c  -> H.span !. fromString c $ inner

{-%
Map the fragment classifiers into css classes.
-}
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

renderPage :: FilePath -> H.Html -> [H.Html] -> String
renderPage css toc mods = renderHtml . H.docTypeHtml $ docHead >> docBody where
    docHead = H.head $ cssLink >> H.title "ProseDoc Generated Module Listing"
    docBody = H.body $ (H.table !. "sections") $ toc <> mconcat mods

    cssLink = H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href (fromString css)

-- | Add an class to an element.
(!.) :: (Attributable h) => h -> H.AttributeValue -> h
elem !. className = elem ! A.class_ className

-- | Add an id to an element.
(!#) :: (Attributable h) => h -> H.AttributeValue -> h
elem !# idName = elem ! A.id idName
