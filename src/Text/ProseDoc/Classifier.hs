{-%
## Forcing a square peg through a round hole

(aka: syntax coloring of Haskell source code using the [`haskell-src-exts`](http://hackage.haskell.org/package/haskell-src-exts)
package)

There are several good syntax highlighting libraries and tools available, so
the simplest thing would have been just to use one and be done with it. However,
none of the syntax highlighters that I could find worked 100% perfectly with
the plethora of language extensions available for modern GHC (`TemplateHaskell`
is particularly tricky), so in a bout of perfectionism I decided I'd try and
make a syntax highlighter that uses a full blown Haskell parser to get
everything right.

If I really wanted to be sure I'm supporting all the syntax extensions currently
available, the right thing to do would be to use the [`GHC API`](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/index.html)
directly so that anything I can compile, I can highlight. However, I have to
admit that the thought of diving into the GHC API absolutely terrifies me (I'll
get to it some day, I promise!), so settled with the next best thing, namely [`haskell-src-exts`](http://hackage.haskell.org/package/haskell-src-exts).

Now, `haskell-src-exts` supports an admirably large portion of the
Haskell syntax, but token classification for syntax highlighting isn't really
one of the targeted use-cases for the library.

* You get a [`Token`](http://hackage.haskell.org/packages/archive/haskell-src-exts/latest/doc/html/Language-Haskell-Exts-Lexer.html#t:Token)
stream from the [lexer](http://hackage.haskell.org/packages/archive/haskell-src-exts/latest/doc/html/Language-Haskell-Exts-Lexer.html)
that contains information such as which words are keywords.
* You get an AST
(thankfully, annotated with position meta-data) from the [parser](http://hackage.haskell.org/packages/archive/haskell-src-exts/latest/doc/html/Language-Haskell-Exts-Parser.html)
for figuring out the context, i.e. whether you are inside a type signature
or a pattern match.
* And finally, you get [comments](http://hackage.haskell.org/packages/archive/haskell-src-exts/1.13.5/doc/html/Language-Haskell-Exts-Comments.html)
(again, tagged with position info) as a separate list.

Which gets us to this module: `Classifier`
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.ProseDoc.Classifier where

import Control.Applicative

import Data.String ()
import Data.Monoid

import Data.Data
import Data.Typeable (cast)
import Data.Generics.Schemes

import Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.Exts.Annotated.Syntax as S

import Text.ProseDoc.Tree
import Text.ProseDoc.Tree.Builder
import Text.ProseDoc.Classifier.Types
import Text.ProseDoc.Classifier.Tokens ()

{-%
The `ASTClassifier` type class is used to process nodes in the annotated AST
produced by `haskell-src-ext` and to build a `Tree` (covered later) of what are
essentially tokens classified (or labeled) according to their context in the
AST.

The `mkTree` function of the type-class operates in the `TreeBuilder` monad
(also covered later), which keeps track of the current source position and the
lists of tokens and comments we haven't yet processed.
-}
class ASTClassifier ast where
    mkTree :: ast -> TreeBuilder (Tree Classifier Printable)
    mkTree = const mempty

{-%
As a generic convenience, we define some helper instances so that we can
process ast elements, tree builders and lists of the aforementionted in a
consistent manner.
-}
instance ASTClassifier (TreeBuilder (Tree Classifier Printable)) where
    mkTree = id

instance ASTClassifier a => ASTClassifier [a] where
    mkTree = fmap mconcat . mapM mkTree

instance ASTClassifier a => ASTClassifier (Maybe a) where
    mkTree Nothing  = mempty
    mkTree (Just a) = mkTree a

{-%
The root node of the AST is always the `Module`. By using the generic traversal
scheme from [`Data.Generics.Schemes`](http://hackage.haskell.org/packages/archive/syb/latest/doc/html/Data-Generics-Schemes.html)
we can avoid most of the trouble of having to write an instance for every
single kind of AST node.

If there are any source code fragments that were not processed by the generic
traversal, we append them with `popRemaining`.
-}
instance ASTClassifier (S.Module SrcSpan) where
    mkTree m = mappend
        <$> everything mappend gTree m
        <*> popRemaining

{-%
`gTree` takes advantage of the `Data.Data` instance of the AST elements and
makes transformations throughout the tree. "Leaf" type nodes like names are
processed using `popAst` and more complex elements delegate to the AST element's
`ASTClassifier` type class instance.
-}
gTree :: Data a => a -> TreeBuilder (Tree Classifier Printable)
gTree (cast -> Just (c :: S.ModulePragma SrcSpan)) = mkTree c
gTree (cast -> Just (c :: S.ImportDecl SrcSpan))   = mkTree c
gTree (cast -> Just (c :: S.Type SrcSpan))         = mkTree c
gTree (cast -> Just (c :: S.QName SrcSpan))        = mkTree c
gTree (cast -> Just (c :: S.Name SrcSpan))         = mkTree c
gTree (cast -> Just c@(S.Con {}))                  = popAst' ConstrName c
gTree (cast -> Just c@(S.PApp _ qn _))             = popAst' ConstrName qn
gTree (cast -> Just c@(S.PRec _ qn _))             = popAst' ConstrName qn
gTree (cast -> Just (c :: S.QOp SrcSpan))          = popAst' InfixOperator c
gTree (cast -> Just c@(S.String {}))               = popAst' StringLit c
gTree (cast -> Just c@(S.TypeSig l names typ))
    =  popPrintablesBefore l
    <> label Signature (mkTree names <> mkTree typ)
gTree _ = mempty

{-%
`label` is an short-hand function for adding a parent classifier to any value
that is an `ASTClassifier` itself.
-}
label :: ASTClassifier a
    => Classifier
    -> a
    -> TreeBuilder (Tree Classifier Printable)
label l a = Label l <$> mkTree a

{-%
`label'` is a variation of `label` which discards all labels from child trees.
This is sometimes useful to prevent an element from gettings several redundant
classifiers.
-}
label' :: ASTClassifier a
    => Classifier
    -> a
    -> TreeBuilder (Tree Classifier Printable)
label' l a = Label l . pruneLabels <$> mkTree a

{-%
`popAst` assigns the given label to the specified, annotated AST element and pops
all code fragments within the AST element's `SrcSpan`.
-}
popAst :: S.Annotated ast
    => Classifier
    -> ast SrcSpan
    -> TreeBuilder (Tree Classifier Printable)
popAst cls ast = popPrintablesBefore l <> label cls (popPrintables l)
    where l = S.ann ast

{-%
`popAst'` is a version of `popAst` which prunes any nested labels.
-}
popAst' :: S.Annotated ast
    => Classifier
    -> ast SrcSpan
    -> TreeBuilder (Tree Classifier Printable)
popAst' cls ast = popPrintablesBefore l <> label' cls (popPrintables l)
    where l = S.ann ast

{-%
The rest of the module consists of `ASTClassifier` instances for  AST nodes
for which we want to do some specific processing i.e. assign specific labels
to some children of the node which cannot be accurately identified just based
on their type (in which case we could process them in `gTree`).

This part of the syntax highlighter is still work-in-progress. We need more
`ASTClassifier` instances to add rest of the syntax coloring.
-}
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
    mkTree i@(S.ImportDecl{..})
        =  popPrintablesBefore importAnn
        <> label ImportDecl
        (  mkTree importModule
        <> mkTree importAs
        <> mkTree importSpecs
        )

instance ASTClassifier (S.ImportSpecList SrcSpan) where
    mkTree d
        = popPrintablesBefore l
        <> everything mappend gTree d
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
        S.TyCon _ (S.Qual _ m n)
            -> mkTree m
            <> do
                within <- currentlyWithin l
                -- Pop the dot in the qualified name separately
                if within
                    then popCustom Punctuation 1
                    else mempty
            <> label' TypeName (mkTree n)
        _ -> label TypeName $ popPrintables l
        where l = S.ann n

instance ASTClassifier (S.QName SrcSpan) where
    mkTree n = popPrintablesBefore l <> case n of
        S.Qual _ m n -> mkTree m <> mkTree n
        _ -> label Name $ popPrintables l
        where l = S.ann n
