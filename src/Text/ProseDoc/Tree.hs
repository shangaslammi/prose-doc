{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.ProseDoc.Tree where

import Data.Monoid
import Data.Foldable (foldMap)

{-%
The idea is to build a tree, where each branch of the tree can add "classifiers"
and leafs of the tree contains actual tokens that are printed. The classifiers
can then be used e.g. by a HTML rendering engine to add CSS class annotations.
-}
data Tree l n where
    Empty  :: Tree l n
    Label  :: l -> Tree l n -> Tree l n
    Branch :: [Tree l n] -> Tree l n
    Leaf   :: n -> Tree l n

{-%
The tree is folded into a monoid depth-first using an abstract function that
takes in two functions. The first is used to process branches and the second
to process leaves.
-}
foldTree :: Monoid r => (l -> r -> r) -> (n -> r) -> Tree l n -> r
foldTree bf lf = go where
    go Empty           = mempty
    go (Label l tree)  = bf l (go tree)
    go (Branch leaves) = foldMap go leaves
    go (Leaf t)        = lf t

{-%
Trees form a monoid. The label type is constrained to `Eq` so that we can
directly merge tree with an identical label.
-}
instance Eq l => Monoid (Tree l n) where
    mempty          = Empty

    mappend Empty b = b
    mappend a Empty = a
    mappend (Label l t) (Label l' t') | l == l' = Label l (t <> t')
    mappend (Branch b) (Branch b') = Branch (b <> b')
    mappend a (Branch b) = Branch (a:b)
    mappend a b = Branch [a,b]

