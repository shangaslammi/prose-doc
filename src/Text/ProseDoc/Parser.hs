{-# LANGUAGE ScopedTypeVariables #-}

module Text.ProseDoc.Parser where

import Control.Applicative ((<$>))
import Control.Error

import Data.List (sort)

import Language.Haskell.Exts (readExtensions)
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Annotated.Syntax as S

import Text.ProseDoc.Tree
import Text.ProseDoc.Tree.Builder
import Text.ProseDoc.Classifier
import Text.ProseDoc.Classifier.Types

parseSourceFile :: FilePath -> EitherT String IO (Tree Classifier Printable)
parseSourceFile fp = do
    src  <- scriptIO $ readFile fp
    hoistEither $ buildTree fp src

buildTree :: FilePath -> String -> Either String (Tree Classifier Printable)
buildTree path src = case parseResult . parseMode <$> readExtensions src of
    Nothing                    -> Left "unable to parse language extensions"
    Just (ParseOk r)           -> return r
    Just (ParseFailed loc msg) -> Left (prettyPrint loc ++ ": " ++ msg)

    where
        parseResult mode = do
            tokens <- lexTokenStreamWithMode mode src
            (ast :: S.Module SrcSpan, comments) <- parseWithComments mode src

            let builder   = mkTree ast
                tree      = runTreeBuilder builder src fragments
                fragments
                    =  sort
                    $  concatMap toFragments tokens
                    ++ concatMap toFragments comments

            return tree

        parseMode exts = defaultParseMode
            { parseFilename = path
            {-% Disabling fixities allows parsing custom operators -}
            , fixities = Just []
            , extensions = exts
            }

