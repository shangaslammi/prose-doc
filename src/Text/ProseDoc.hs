{-%
# Text.ProseDoc

`Text.ProseDoc` is a tool that reads markdown formatted comments from a
Haskell source file and composes the comments and the associated source
into a HTML document where the prose and source flow side by side in sync.

It can be seen as an alternative way to write literal Haskell code.
-}

module Text.ProseDoc where

import Control.Monad ((<=<))
import Control.Error

import Text.Pandoc.SelfContained

{-%
## Lexing Haskell Source Code

We use the [`haskell-src-exts`](http://hackage.haskell.org/package/haskell-src-exts)
package to lex the Haskell code. This lets us correctly parse and syntax highlight
almost all the syntactic extensions supported by modern GHC.
-}
import Text.ProseDoc.Rendering
import Text.ProseDoc.Parser

testClassifier :: IO ()
testClassifier = runScript $ do
    tree <- parseSourceFile "src/Text/ProseDoc.hs"
    let sections = extractSections tree
    -- scriptIO $ print tree
    -- scriptIO $ print sections

    scriptIO
        $ writeFile "ProseDoc.html"
        <=< makeSelfContained (Just "css")
        . renderPage
        $ sections

