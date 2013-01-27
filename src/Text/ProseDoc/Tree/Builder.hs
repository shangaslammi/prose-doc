{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Text.ProseDoc.Tree.Builder where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

import Language.Haskell.Exts.SrcLoc

import Text.ProseDoc.Tree
import Text.ProseDoc.Classifier.Types

import Debug.Trace

newtype TreeBuilder a = TreeBuilder (StateT (String, Pos) (State [Fragment]) a)
    deriving (Functor, Applicative, Monad)

instance Monoid (TreeBuilder (Tree Classifier Printable)) where
    mempty  = return mempty
    mappend = liftM2 mappend

{-%
Given a `TreeBuilder`, the origina lsource code and classified fragments, create
a tree of classified, printable elements.
-}
runTreeBuilder :: TreeBuilder a -> String -> [Fragment] -> a
runTreeBuilder (TreeBuilder bldr) src =
    evalState $ evalStateT bldr (src,(1,1))

{-%
Pop all fragments that are before the given position.
-}
popFragments :: Pos -> TreeBuilder [Fragment]
popFragments pos = TreeBuilder $ do
    fragments <- lift get
    let (include, exclude) = span ((<= pos).srcSpanEnd.fst) fragments
        (include', exclude') = case exclude of
            (e@(p,_):_) | srcSpanStart p < pos ->
                let (e',e'') = breakFragment pos e
                in (include ++ [e'], e'':exclude)
            _ -> (include, exclude)
    lift $ put exclude'
    -- return $ trace (show (pos, include)) $ include
    return include'

{-%
Sometimes we need to manually tweak some fragments. `popCustom` lets us
pop a custom fragment with given classifier and length from the current
source position.
-}
popCustom :: Classifier -> Int -> TreeBuilder (Tree Classifier Printable)
popCustom cls len = do
    (ln,col) <- TreeBuilder $ gets snd
    fragmentToTree (SrcSpan "" ln col ln (col + len), cls)

{-%
Break a fragment into two parts at position.
-}
breakFragment :: Pos -> Fragment -> (Fragment, Fragment)
breakFragment (ln,col) (l, cls) = ((loc, cls), (loc', cls)) where
    loc  = l { srcSpanEndColumn = col, srcSpanEndLine = ln }
    loc' = l { srcSpanStartColumn = col, srcSpanStartLine = ln }

{-%
Pop fragments from stack and structure them into a tree.
-}
popPrintables :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
popPrintables loc | isNullSpan loc = return Empty
popPrintables loc =
    mconcat <$> (popFragments (srcSpanEnd loc) >>= mapM fragmentToTree)

popPrintablesBefore :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
popPrintablesBefore loc = {- trace (show loc) $-}
    (mconcat <$> (popFragments (srcSpanStart loc) >>= mapM fragmentToTree))
    <> leftOvers
    where
        leftOvers = do
            fragments <- TreeBuilder $ lift get
            case fragments of
                []    -> return mempty
                ((loc',_):_) -> beforeToTree loc'

popRemaining :: TreeBuilder (Tree Classifier Printable)
popRemaining = mconcat <$> (popAllFragments >>= mapM fragmentToTree) where
    popAllFragments = TreeBuilder (lift get <* lift (put []))


{-%
Pop source code until the end of given fragment span and structure
the fragment into a classified tree.
-}
fragmentToTree :: Fragment -> TreeBuilder (Tree Classifier Printable)
fragmentToTree (loc, cls) = beforeToTree loc <> fragment where
    fragment = Label cls . Leaf <$> splitSpan (srcSpanEnd loc)

beforeToTree :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
beforeToTree loc = do
    pre <- splitSpan (srcSpanStart loc)
    case pre of
        "" -> return mempty
        _  -> return $ Leaf pre

splitSpan :: Pos -> TreeBuilder String
splitSpan (ln', col') = TreeBuilder $ unwrapWriter go where
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
        tell (s ++)
        tell ('\n' :)

    popChars n = do
        (src, (ln, col)) <- lift get
        let (s, src') = splitAt n src
        put (src', (ln, col+n))
        tell (s ++)
