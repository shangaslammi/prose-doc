{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.ProseDoc.Tree where

import Data.Monoid
import Data.Foldable (foldMap)

{-%
A tree data type holds labels, branches and leaf nodes. A label classifies all
branches and leaves below it.
-}
data Tree l n where
    Empty  :: Tree l n
    Label  :: l -> Tree l n -> Tree l n
    Branch :: [Tree l n] -> Tree l n
    Leaf   :: n -> Tree l n
    deriving (Show)

{-%
The tree is folded into a monoid, depth-first using an abstract function that
takes in two functions. The first is used to process labels and the second
to process leaf data.
-}
foldTree :: Monoid r => (l -> r -> r) -> (n -> r) -> Tree l n -> r
foldTree bf lf = go where
    go Empty           = mempty
    go (Label l tree)  = bf l (go tree)
    go (Branch leaves) = foldMap go leaves
    go (Leaf t)        = lf t

{-%
Trees form a monoid so that the two trees are inserted as siblings into a branch.
-}
instance Monoid (Tree l n) where
    mempty          = Empty

    mappend Empty b = b
    mappend a Empty = a
    mappend (Branch b) (Branch b') = Branch (b <> b')
    mappend a (Branch b) = Branch (a:b)
    mappend a b = Branch [a,b]

{-%
Remove all label nodes from the tree.
-}
pruneLabels :: Tree l n -> Tree l n
pruneLabels t = case t of
    Label _ t' -> t'
    Branch bs  -> Branch $ map pruneLabels bs
    t'         -> t'

{-%
Remove parts of the tree that don't contain any leaf nodes.
-}
pruneEmptyBranches :: Tree l n -> Tree l n
pruneEmptyBranches t = go t where
    go t = case t of
        Empty     -> Empty
        Branch bs -> case filter notEmpty . map go $ bs of
            []  -> Empty
            bs' -> Branch bs'
        Label l t -> case go t of
            Empty -> Empty
            t'    -> Label l t'
        n         -> n

    notEmpty Empty = False
    notEmpty _     = True


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
            | test l    -> (Empty, Just (Label l t'), Empty)
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
    go sep Empty = (sep, Empty) : []
    go sep t = case (sep, l) of
        (Nothing, Empty) -> go sep' r
        _                -> (sep, l) : go sep' r

        where (l, sep', r) = breakTree test t
