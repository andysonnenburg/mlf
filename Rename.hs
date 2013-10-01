{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Rename (RenameError (..), rename) where

import Control.Applicative
import Control.Comonad.Env (Comonad (..), ComonadEnv)
import Control.Monad.Error
import Control.Monad.Reader

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map
import Data.Monoid
import Data.Traversable (Traversable, for)

import Text.PrettyPrint.Free

import Name
import Supply
import Type.Syntactic

data RenameError a = NotFound a deriving Show

instance Pretty a => Pretty (RenameError a) where
  pretty (NotFound a) = pretty a <+> text "not" <+> text "found"

rename :: ( Eq a
          , Hashable a
          , Traversable w
          , ComonadEnv e w
          , Applicative m
          , MonadError (w (RenameError a)) m
          , MonadSupply Int m
          ) => w (PolyType w a) -> m (w (PolyType w (Name a)))
rename = flip runReaderT mempty . renamePoly
  where
    renamePoly = fix $ \ rec w -> for w $ \ case
      Mono t -> Mono . extract <$> renameMono (t <$ w)
      Bot -> return Bot
      Forall x bf a b -> do
        x' <- Name (Just x) <$> supply
        Forall x' bf <$> rec a <*> local (Map.insert x x') (rec b)
    renameMono = fix $ \ rec w -> for w $ \ case
      Var x -> asks (Map.lookup x) >>= \ case
        Nothing -> throwError $ NotFound x <$ w
        Just x' -> return $ Var x'
      Arr a b -> Arr <$> rec a <*> rec b
