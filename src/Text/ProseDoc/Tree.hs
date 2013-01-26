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
    deriving (Show)

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
Trees form a monoid so that the two trees are merged into the same branch.
-}
instance Monoid (Tree l n) where
    mempty          = Empty

    mappend Empty b = b
    mappend a Empty = a
    mappend (Branch b) (Branch b') = Branch (b <> b')
    mappend a (Branch b) = Branch (a:b)
    mappend a b = Branch [a,b]

{-%
A tree can be split into parts based a label test. The split off parts will
contain the same labels when walking the tree spine as the part they were cut
off from.
-}
breakTree :: (l -> Bool) -> Tree l n -> (Tree l n, Maybe (Tree l n), Tree l n)
breakTree test = go where
    go t = case t of
        Empty -> (Empty, Nothing, Empty)
        Label l t'
            | test l    -> (Empty, Just (Label l t'), Empty)
            | otherwise -> (Label l a, sep, b')
            where
                (a, sep, b) = go t'
                b' = case sep of
                    Nothing -> Empty
                    _       -> Label l b
        Leaf n    -> (Leaf n, Nothing, Empty)
        Branch bs -> case sep of
            Nothing -> (Branch bs, Nothing, Empty)
            _       -> (mconcat a, sep, mconcat b)
            where (a, sep, b) = gob bs

    gob [] = ([], Nothing, [])
    gob (b:bs) = case sep of
        Nothing -> (l:b', sep', bs')
        _       -> (l:[], sep, r:bs)
        where
            (l, sep, r) = go b
            (b', sep', bs') = gob bs

{-%
Version of `breakTree` which splits the tree into a list of tree sections.
-}
splitTree :: (l -> Bool) -> Tree l n -> [(Maybe (Tree l n), Tree l n)]
splitTree test = go Nothing where
    go Nothing Empty = []
    go sep Empty     = (sep, Empty) : []
    go sep t = (sep, l) : go sep' r where
        (l, sep', r) = breakTree test t


testSplit = splitTree (=='e') t where
    t = Branch [b1,b2,b3]
    b1 = Label '1' $ Branch [a,b,c]
    a = Label 'a' (Leaf 1)
    b = Label 'b' (Leaf 2)
    c = Label 'c' (Leaf 3)
    b2 = Label '2' $ Branch [d,e,f]
    d = Label 'd' (Leaf 4)
    e = Label 'e' (Leaf 5)
    f = Label 'f' (Leaf 6)
    b3 = Label '3' $ Branch [g,h,i]
    g = Label 'g' (Leaf 7)
    h = Label 'h' (Leaf 8)
    i = Label 'i' (Leaf 9)
