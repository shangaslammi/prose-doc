{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Text.ProseDoc.Tree.Builder where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

import Data.Char
import Data.Monoid

import Language.Haskell.Exts.SrcLoc

import Text.ProseDoc.Tree
import Text.ProseDoc.Classifier.Types

newtype TreeBuilder a = TreeBuilder (StateT (String, (Int, Int)) (State [Fragment]) a)
    deriving (Functor, Applicative, Monad)

{-%
Given a `TreeBuilder`, the origina lsource code and classified fragments, create
a tree of classified, printable elements.
-}
runTreeBuilder :: TreeBuilder a -> String -> [Fragment] -> a
runTreeBuilder (TreeBuilder bldr) src fragments =
    evalState (evalStateT bldr (src,(1,1))) fragments

{-%
Pop all fragments that are within (or before) the given span.
-}
popFragments :: SrcSpan -> TreeBuilder [Fragment]
popFragments loc = TreeBuilder $ do
    fragments <- lift get
    let (include, exclude) = span ((< srcSpanEnd loc).srcSpanStart.fst) fragments
    lift $ put exclude
    return include

{-%
Pop fragments from stack and structure them into a tree.
-}
popPrintables :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
popPrintables loc | isNullSpan loc = return Empty
popPrintables loc =
    mconcat <$> (popFragments loc >>= mapM fragmentToTree)


{-%
Pop source code until the end of given fragment span and structure
the fragment into a classified tree.
-}
fragmentToTree :: Fragment -> TreeBuilder (Tree Classifier Printable)
fragmentToTree (loc, cls) = Label cls . Leaf <$> splitSpan loc


splitSpan :: SrcSpan -> TreeBuilder String
splitSpan (srcSpanEnd -> (ln', col')) = TreeBuilder $ unwrapWriter go where
    unwrapWriter = fmap ($"") . execWriterT

    go = do
        (_, (ln, col)) <- get
        let action
                | ln < ln'   = popLine >> go
                | col < col' = popChars (col'-col)
                | otherwise  = return ()
        action

    popLine = do
        (src, (ln, col)) <- lift get
        let (s, tail -> src') = break (=='\n') src
        lift $ put (src',(ln+1, 1))
        tell (++ (s ++ "\n"))


    popChars n = do
        (src, (ln, col)) <- lift get
        let (s, src') = splitAt n src
        put (src', (ln, col+n))
        tell (++ s)

