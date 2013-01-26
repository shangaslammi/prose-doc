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

import Control.Applicative

import Data.String
import Data.Foldable
import Data.Monoid

import Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.Exts.Annotated.Syntax as S

import Text.ProseDoc.Tree
import Text.ProseDoc.Tree.Builder
import Text.ProseDoc.Classifier.Types
import Text.ProseDoc.Classifier.Tokens


class ASTClassifier a where
    mkTree :: a -> TreeBuilder (Tree Classifier Printable)
    mkTree = const mempty

instance ASTClassifier a => ASTClassifier [a] where
    mkTree = fmap mconcat . mapM mkTree

instance ASTClassifier a => ASTClassifier (Maybe a) where
    mkTree Nothing  = mempty
    mkTree (Just a) = mkTree a

instance ASTClassifier (S.Module SrcSpan) where
    mkTree (S.Module l hd pragmas imports decls) = do
        popPrintablesBefore l
        mconcat [mkTree hd, mkTree pragmas, mkTree imports, mkTree decls]

instance ASTClassifier (S.ModuleHead SrcSpan) where
    mkTree (S.ModuleHead l name warning exports) = do
        popPrintablesBefore l
        Label ModuleHead <$> mconcat [mkTree name, mkTree warning, mkTree exports]

instance ASTClassifier (S.ModulePragma SrcSpan) where
instance ASTClassifier (S.ImportDecl SrcSpan) where
instance ASTClassifier (S.Decl SrcSpan) where
instance ASTClassifier (S.ModuleName SrcSpan) where
instance ASTClassifier (S.WarningText SrcSpan) where
instance ASTClassifier (S.ExportSpecList SrcSpan) where

