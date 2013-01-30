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

{-%
A `TreeBuilder` is a helper monad for associating AST elements with the position
tagged streams of tokens and comments.

The first state transformer tracks the remaining source code and the current
line/row position in the file. The inner state monad keeps a stack of source
code fragments (tokens and comments) which are popped from the stack by the
AST element that covers the source location.
-}
newtype TreeBuilder a = TreeBuilder (StateT (String, Pos) (State [Fragment]) a)
    deriving (Functor, Applicative, Monad)

{-%
For convenience, we define a monoid instance for tree-producing `TreeBuilders`
so that we can directly mappend two monadic operations.
-}
instance Monoid (TreeBuilder (Tree Classifier Printable)) where
    mempty  = return mempty
    mappend = liftM2 mappend

{-%
Given a `TreeBuilder`, the origina lsource code and classified fragments, create
a tree of classified, printable elements.
-}
runTreeBuilder
    :: TreeBuilder (Tree Classifier Printable)
    -> String
    -> [Fragment]
    -> Tree Classifier Printable
runTreeBuilder (TreeBuilder bldr) src =
    evalState $ evalStateT bldr (src,(1,1))

{-%
Pop all fragments that are before the given position.
-}
popFragments :: Pos -> TreeBuilder [Fragment]
popFragments pos = TreeBuilder $ do
    fragments <- lift get
    let (include, exclude) = span ((<= pos).srcSpanEnd.fst) fragments
        (include', exclude') = case exclude of
            (e@(p,_):_) | srcSpanStart p < pos ->
                let (e',e'') = breakFragment pos e
                in (include ++ [e'], e'':exclude)
            _ -> (include, exclude)
    lift $ put exclude'
    return include'

{-%
Sometimes we need to manually tweak some fragments. `popCustom` lets us
pop a custom fragment with given classifier and length from the current
source position.
-}
popCustom :: Classifier -> Int -> TreeBuilder (Tree Classifier Printable)
popCustom cls len = do
    (ln,col) <- TreeBuilder $ gets snd
    fragmentToTree (SrcSpan "" ln col ln (col + len), cls)

{-%
Break a fragment into two parts at position.
-}
breakFragment :: Pos -> Fragment -> (Fragment, Fragment)
breakFragment (ln,col) (l, cls) = ((loc, cls), (loc', cls)) where
    loc  = l { srcSpanEndColumn = col, srcSpanEndLine = ln }
    loc' = l { srcSpanStartColumn = col, srcSpanStartLine = ln }

{-%
Pop fragments that are located inside the given `SrcSpan` from the stack and
structure them into a tree.
-}
popPrintables :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
popPrintables loc | isNullSpan loc = return Empty
popPrintables loc =
    mconcat <$> (popFragments (srcSpanEnd loc) >>= mapM fragmentToTree)

{-%
Pop fragments that are located _before_ the given `SrcSpan` from the stack and
structure them into a tree.
-}
popPrintablesBefore :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
popPrintablesBefore loc =
    (mconcat <$> (popFragments (srcSpanStart loc) >>= mapM fragmentToTree))
    <> leftOvers
    where
        leftOvers = do
            fragments <- TreeBuilder $ lift get
            case fragments of
                []    -> return mempty
                ((loc',_):_) -> beforeToTree loc'

{-%
Pop all remaining source fragments from the stack.
-}
popRemaining :: TreeBuilder (Tree Classifier Printable)
popRemaining = mconcat <$> (popAllFragments >>= mapM fragmentToTree) where
    popAllFragments = TreeBuilder (lift get <* lift (put []))

{-%
Check if the current source location is within the given span.
-}
currentlyWithin :: SrcSpan -> TreeBuilder Bool
currentlyWithin loc = TreeBuilder $ fmap (< srcSpanEnd loc) $ gets snd

{-%
Pop source code until the end of given fragment span and structure
the fragment into a classified tree.
-}
fragmentToTree :: Fragment -> TreeBuilder (Tree Classifier Printable)
fragmentToTree (loc, cls) = beforeToTree loc <> fragment where
    fragment = Label cls . Leaf <$> extractSource (srcSpanEnd loc)

beforeToTree :: SrcSpan -> TreeBuilder (Tree Classifier Printable)
beforeToTree loc = do
    pre <- extractSource (srcSpanStart loc)
    case pre of
        "" -> return mempty
        _  -> return $ Leaf pre
{-%
Retrieve source code up until the given line/row position as a string.
-}
extractSource :: Pos -> TreeBuilder String
extractSource (ln', col') = TreeBuilder $ unwrapWriter go where
    unwrapWriter = fmap ($"") . execWriterT

    go = do
        (_, (ln, col)) <- get
        let action
                | ln < ln'   = popLine >> go
                | col < col' = popChars (col'-col)
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
