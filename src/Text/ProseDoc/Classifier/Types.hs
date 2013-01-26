
module Text.ProseDoc.Classifier.Types where

import Language.Haskell.Exts.SrcLoc

import Text.ProseDoc.Tree

data Printable   = Printable SrcSpan String

data Classifier
    = Module
    | ModuleHead
    | ModulePragma
    | ImportDecl
    | ModuleName
    | ValueName
    | ConstrName
    | Keyword
    | Punctuation
    | QuasiQuote
    | THQuote
    | THEscape
    | Other
    | BlockComment
    | LineComment
    deriving (Show, Eq, Ord)

type Fragment = (SrcSpan, Classifier)

class SourceFragment f where
    toFragment :: f -> Fragment


data TreeBuilder a = TreeBuilder a

{-%
Given a `TreeBuilder`, source code lines and classified fragments, create
a tree of classified, printable elements.
-}
runTreeBuilder :: TreeBuilder a -> [String] -> [Fragment] -> Tree Classifier Printable
runTreeBuilder = undefined

{-%
Pop fragments from stack and structure them into a tree.
-}
popPrintables :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
popPrintables = undefined
