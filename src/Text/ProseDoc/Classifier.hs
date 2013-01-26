{-%
## Token Classification

The idea of the token classification is to take an abstract syntax tree
and turn it into a "presentational syntax tree" which contains all the
information needed to print out a syntax highlighted version of the original
source, but nothing more.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.ProseDoc.Classifier where

import Prelude hiding (concatMap)


import Data.String
import Data.Foldable (foldMap)

import qualified Language.Haskell.Exts.Annotated.Syntax as S

import Text.ProseDoc.Tree
import Text.ProseDoc.Classifier.Types
import Text.ProseDoc.Classifier.Tokens


{-
instance SrcInfo l => MkTree Classifier Printable (S.Module l) where
    mkTree (S.Module _ hd pragmas imports decls) =
        Branch Module
            $ foldMap (return.mkTree) hd
            ++ map mkTree pragmas
            ++ map mkTree imports
            ++ map mkTree decls
-}
