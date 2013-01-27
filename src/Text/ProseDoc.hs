{-%
# Text.ProseDoc

`Text.ProseDoc` is a tool that reads markdown formatted comments from a
Haskell source file and composes the comments and the associated source
into a HTML document where the prose and source flow side by side in sync.

The concept is blatantly borrowed from the CoffeeScript tool [docco](http://jashkenas.github.com/docco/).

ProseDoc can be seen as an alternative way to write and format literal Haskell
code. However, the main motivation behind writing ProseDoc was simply that it
seemed like an interesting project to tinker with.
-}
module Text.ProseDoc where

import Control.Applicative ((<$>))
import Control.Monad       ((<=<), filterM, forM)
import Control.Error

import Data.Monoid (mempty)
import Data.List   (sort, isPrefixOf)

import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath  ((</>), takeExtension, makeRelative)

import Text.Pandoc.SelfContained

import Text.ProseDoc.Rendering
import Text.ProseDoc.Parser

{-%
The current version can generate a document either from a single source file
or a hierarchical module structure.
-}
generatePage :: FilePath -> IO String
generatePage path = do
    isFile <- doesFileExist path
    if isFile then processSingle path else processDirectory path

processSingle :: FilePath -> IO String
processSingle path = do
    t <- runScript (parseSourceFile path)
{-%
For a single document, we leave out the TOC and simply format the given
module. The [`makeSelfContained`](http://hackage.haskell.org/packages/archive/pandoc/latest/doc/html/Text-Pandoc-SelfContained.html#v:makeSelfContained)
function from [`pandoc`](http://hackage.haskell.org/package/pandoc)
is used to embed the style information from an external css file.
-}
    makeSelfContained (Just "css")
        $ renderPage mempty
        $ [moduleToHtml (path, t)]

findModules :: FilePath -> IO [FilePath]
findModules root = do
    isFile <- doesFileExist root
    if isFile
{-%
For directories, we walk through all subdirectories and gather
all files with the extension `.hs`.
-}
        then return $ if takeExtension root == ".hs" then [root] else []
        else fmap concat
            $   mapM findModules
            =<< map (root </>) . filter (not . isPrefixOf ".")
            <$> getDirectoryContents root

processDirectory :: FilePath -> IO String
processDirectory path = do
{-%
Currently, the modules are presented in alphabetical order but this should
be user configurable so that more relevant modules can be made to appear
first.
-}
    mods <- sort . map (makeRelative path) <$> findModules path

    htmls <- forM mods $ \m -> do
        t <- runScript $ parseSourceFile (path </> m)
        return $ moduleToHtml (m, t)

    let toc = htmlTOC mods

    makeSelfContained (Just "css") $ renderPage toc htmls


