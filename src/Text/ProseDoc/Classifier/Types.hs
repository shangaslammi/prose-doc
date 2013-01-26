
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
    | ProseComment String
    deriving (Show, Eq, Ord)

type LineNo = Int
type Column = Int
type Pos = (LineNo, Column)

type Printable = String
type Fragment  = (SrcSpan, Classifier)

class SourceFragment f where
    toFragment :: f -> Fragment


