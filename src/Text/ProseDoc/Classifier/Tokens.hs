
{-# LANGUAGE FlexibleInstances #-}

module Text.ProseDoc.Classifier.Tokens where

import Control.Arrow ((&&&))

import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Comments

import Text.ProseDoc.Classifier.Types

instance SourceFragment (Loc Token) where
    toFragment = (loc &&& classifyToken . unLoc)

instance SourceFragment (Comment) where
    toFragment (Comment block loc _) = (loc, comment) where
        comment
            | block     = BlockComment
            | otherwise = LineComment

classifyToken :: Token -> Classifier
classifyToken t
    | punctuation t  = Punctuation
    | keyword t      = Keyword
    | thQuote t      = THQuote
    | thEscape t     = THEscape
    | thQuasiQuote t = QuasiQuote
    | otherwise      = Other

thQuote :: Token -> Bool
thQuote t = elem t
    [ THExpQuote
    , THPatQuote
    , THDecQuote
    , THTypQuote
    , THCloseQuote
    , THVarQuote
    , THTyQuote
    ]

thEscape :: Token -> Bool
thEscape (THIdEscape _) = True
thEscape THParenEscape  = True
thEscape _ = False

thQuasiQuote :: Token -> Bool
thQuasiQuote (THQuasiQuote _) = True
thQuasiQuote _ = False

punctuation :: Token -> Bool
punctuation = flip elem
    [ LeftParen
    , RightParen
    , LeftHashParen
    , RightHashParen
    , LeftCurlyBar
    , RightCurlyBar
    , SemiColon
    , LeftCurly
    , RightCurly
    , VRightCurly
    , LeftSquare
    , RightSquare
    , Comma
    , Underscore
    , BackQuote
    , Dot
    , DotDot
    , Colon
    , DoubleColon
    , Equals
    , Backslash
    , Bar
    , LeftArrow
    , RightArrow
    , At
    , Tilde
    , DoubleArrow
    , Minus
    , Exclamation
    , Star
    , LeftArrowTail
    , RightArrowTail
    , LeftDblArrowTail
    , RightDblArrowTail
    ]

keyword :: Token -> Bool
keyword = flip elem
    [ KW_As
    , KW_By
    , KW_Case
    , KW_Class
    , KW_Data
    , KW_Default
    , KW_Deriving
    , KW_Do
    , KW_MDo
    , KW_Else
    , KW_Family
    , KW_Forall
    , KW_Group
    , KW_Hiding
    , KW_If
    , KW_Import
    , KW_In
    , KW_Infix
    , KW_InfixL
    , KW_InfixR
    , KW_Instance
    , KW_Let
    , KW_Module
    , KW_NewType
    , KW_Of
    , KW_Proc
    , KW_Rec
    , KW_Then
    , KW_Type
    , KW_Using
    , KW_Where
    , KW_Qualified
    , KW_Foreign
    , KW_Export
    , KW_Safe
    , KW_Unsafe
    , KW_Threadsafe
    , KW_StdCall
    , KW_CCall
    , KW_CPlusPlus
    , KW_DotNet
    , KW_Jvm
    , KW_Js]
