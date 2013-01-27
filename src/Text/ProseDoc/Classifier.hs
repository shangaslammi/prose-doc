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
{-# LANGUAGE RecordWildCards #-}

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

import Debug.Trace

label :: ASTClassifier a => Classifier -> a -> TreeBuilder (Tree Classifier Printable)
label l a = Label l <$> mkTree a

{-%
`label'` is a variation of `label` which discards all labels from child trees.
This is sometimes useful to prevent an element from gettings several redundant
classifiers.
-}
label' :: ASTClassifier a => Classifier -> a -> TreeBuilder (Tree Classifier Printable)
label' l a = Label l . pruneLabels <$> mkTree a

class ASTClassifier a where
    mkTree :: a -> TreeBuilder (Tree Classifier Printable)
    mkTree = const mempty

instance ASTClassifier (TreeBuilder (Tree Classifier Printable)) where
    mkTree = id

instance ASTClassifier a => ASTClassifier [a] where
    mkTree = fmap mconcat . mapM mkTree

instance ASTClassifier a => ASTClassifier (Maybe a) where
    mkTree Nothing  = mempty
    mkTree (Just a) = mkTree a

instance ASTClassifier (S.Module SrcSpan) where
    mkTree (S.Module l hd pragmas imports decls) =
        mconcat [mkTree pragmas, mkTree hd, mkTree imports, mkTree decls, popRemaining]

instance ASTClassifier (S.ModuleHead SrcSpan) where
    mkTree (S.ModuleHead l name warning exports) =
        popPrintablesBefore l
        <> label ModuleHead [mkTree name, mkTree warning, mkTree exports]

instance ASTClassifier (S.ModulePragma SrcSpan) where
    mkTree p = case p of
        S.LanguagePragma l names ->
            popPrintablesBefore l
            <> (Label ModulePragma <$> mkTree names)
        S.OptionsPragma l _ _ ->
            popPrintablesBefore l
            <> (Label ModulePragma <$> popPrintables l)
        S.AnnModulePragma l _ ->
            popPrintablesBefore l
            <> (Label ModulePragma <$> popPrintables l)


instance ASTClassifier (S.ImportDecl SrcSpan) where
    mkTree (S.ImportDecl{..})
        =  popPrintablesBefore importAnn
        <> mkTree importModule
        <> mkTree importAs

instance ASTClassifier (S.Decl SrcSpan) where
    mkTree d = case d of
        S.TypeSig l names typ
            -> popPrintablesBefore l
            <> label Signature (mkTree names <> mkTree typ)

        _ -> popPrintablesBefore l <> popPrintables l
        where l = S.ann d

instance ASTClassifier (S.ModuleName SrcSpan) where
    mkTree (S.ModuleName l' s)
        = popPrintablesBefore l
        <> label ModuleName (popPrintables l)
        where
            -- ModuleName has invalid span length, so recalculate it from
            -- the actual name.
            SrcSpan {..} = l'
            l = l' { srcSpanEndColumn = srcSpanStartColumn + length s }

instance ASTClassifier (S.WarningText SrcSpan) where
instance ASTClassifier (S.ExportSpecList SrcSpan) where

instance ASTClassifier (S.Name SrcSpan) where
    mkTree n = popPrintablesBefore l <> label Name (popPrintables l)
        where l = S.ann n

instance ASTClassifier (S.Type SrcSpan) where
    mkTree n = popPrintablesBefore l <> case n of
        S.TyFun _ a b -> mkTree a <> mkTree b
        S.TyApp _ a b -> mkTree a <> mkTree b
        S.TyList _ a  -> mkTree a
        S.TyTuple _ _ a -> mkTree a
        S.TyParen _ a -> mkTree a
        S.TyCon _ (S.Qual _ m n) -> mkTree m <> label TypeName (popPrintables l)
        _ -> label TypeName $ popPrintables l
        where l = S.ann n

instance ASTClassifier (S.QName SrcSpan) where
    mkTree n = popPrintablesBefore l <> case n of
        S.Qual _ m n -> mkTree m <> mkTree n
        _ -> label Name $ popPrintables l
        where l = S.ann n
