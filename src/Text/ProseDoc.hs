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

import Data.List (sort, isPrefixOf)

import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath  ((</>), takeExtension, makeRelative)

import Text.Pandoc.SelfContained

import Text.ProseDoc.Rendering
import Text.ProseDoc.Parser

generatePage :: FilePath -> IO String
generatePage path = do
    isFile <- doesFileExist path

    mods <- sort <$> if isFile
        then return [path]
        else map (makeRelative path) <$> findModules path

    htmls <- forM mods $ \m -> do
        t <- runScript $ parseSourceFile (path </> m)
        return $ moduleToHtml (m, t)

    let toc = htmlTOC mods

    makeSelfContained (Just "css") $ renderPage toc htmls


findModules :: FilePath -> IO [FilePath]
findModules root = do
    isFile <- doesFileExist root
    if isFile
        then return $ if takeExtension root == ".hs" then [root] else []
        else fmap concat
            $   mapM findModules
            =<< map (root </>) . filter (not . isPrefixOf ".")
            <$> getDirectoryContents root
