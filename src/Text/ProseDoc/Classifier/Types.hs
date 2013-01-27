
module Text.ProseDoc.Classifier.Types where

import Language.Haskell.Exts.SrcLoc

data Classifier
    = ModuleHead
    | ModulePragma
    | ImportDecl
    | ModuleName
    | ValueName
    | ConstrName
    | TypeName
    | Pragma
    | Name
    | Keyword
    | Punctuation
    | SpecPunctuation
    | Braces
    | QuasiQuote
    | THQuote
    | THEscape
    | Other
    | BlockComment
    | LineComment
    | ProseComment String
    | Signature
    | InfixOperator
    | StringLit
    deriving (Show, Eq, Ord)

type LineNo = Int
type Column = Int
type Pos = (LineNo, Column)

type Printable = String
type Fragment  = (SrcSpan, Classifier)

class SourceFragment f where
    toFragments :: f -> [Fragment]

isProse :: Classifier -> Bool
isProse (ProseComment _) = True
isProse _ = False
