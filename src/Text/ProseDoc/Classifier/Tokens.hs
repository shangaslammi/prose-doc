
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Text.ProseDoc.Classifier.Tokens where

import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Comments

import Text.ProseDoc.Classifier.Types

instance SourceFragment (Loc Token) where
    toFragments lt
        | cls == Pragma
            = (pos `setLen` 3, Punctuation)
            : (pos `moveCol` 3, Pragma)
            : []
        | otherwise     = (pos, cls) : []
        where
            pos   = loc lt
            token = unLoc lt
            cls   = classifyToken token
            moveCol src@(SrcSpan {..}) delta = src
                { srcSpanStartColumn  = srcSpanStartColumn + delta }
            setLen  src@(SrcSpan {..}) len = src
                { srcSpanEndColumn = srcSpanStartColumn + len }

instance SourceFragment Comment where
    toFragments (Comment block loc txt) = (loc', comment) : [] where
        loc' = case txt of
            '%':_ -> loc
                { srcSpanStartColumn = 1
                , srcSpanEndColumn   = 1
                , srcSpanEndLine     = srcSpanEndLine + 1
                }
            _ -> loc

        SrcSpan {..} = loc
        comment
            | block = case txt of
                '%':prose     -> ProseComment prose
                _             -> BlockComment
            | otherwise = LineComment

classifyToken :: Token -> Classifier
classifyToken t
    | braces t              = Braces
    | specialPunctuation t  = SpecPunctuation
    | punctuation t         = Punctuation
    | keyword t             = Keyword
    | pragma t              = Pragma
    | thQuote t             = THQuote
    | thEscape t            = THEscape
    | thQuasiQuote t        = QuasiQuote
    | otherwise             = Other

thQuote :: Token -> Bool
thQuote = flip elem
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

braces :: Token -> Bool
braces = flip elem
    [ LeftParen
    , RightParen
    , LeftHashParen
    , RightHashParen
    , LeftCurlyBar
    , RightCurlyBar
    , LeftCurly
    , RightCurly
    , VRightCurly
    , LeftSquare
    , RightSquare
    ]

specialPunctuation :: Token -> Bool
specialPunctuation = flip elem
    [ BackQuote
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

punctuation :: Token -> Bool
punctuation = flip elem
    [ SemiColon
    , Comma
    , Underscore
    , Dot
    , DotDot
    -- , PragmaEnd
    ]

pragma :: Token -> Bool
pragma t = case t of
    RULES               -> True
    INLINE _            -> True
    INLINE_CONLIKE      -> True
    SPECIALISE          -> True
    SPECIALISE_INLINE _ -> True
    SOURCE              -> True
    DEPRECATED          -> True
    WARNING             -> True
    SCC                 -> True
    GENERATED           -> True
    CORE                -> True
    UNPACK              -> True
    OPTIONS _           -> True
    LANGUAGE            -> True
    ANN                 -> True
    _                   -> False

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
    , KW_Js
    ]
