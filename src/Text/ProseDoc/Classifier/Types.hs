
module Text.ProseDoc.Classifier.Types where

import Language.Haskell.Exts.SrcLoc

data Classifier
    = ModuleHead
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

type Printable = String
type Fragment  = (SrcSpan, Classifier)

class SourceFragment f where
    toFragment :: f -> Fragment


