{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Rename (RenameError (..), rename) where

import Control.Applicative
import Control.Comonad.Env (ComonadEnv)
import Control.Monad.Error
import Control.Monad.Reader

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map
import Data.Monoid
import Data.Traversable (Traversable, for, traverse)

import Name
import Supply
import Type.Syntactic

data RenameError a = NotFound a deriving Show

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
    renamePoly = fix $ \ rec -> traverse $ \ case
      Mono t -> Mono <$> renameMono t
      Bot -> return Bot
      Forall x bf a b -> do
        x' <- Name (Just x) <$> supply
        Forall x' bf <$> rec a <*> local (Map.insert x x') (rec b)
    renameMono = fix $ \ rec w -> for w $ \ case
      Var x -> do
        Map.lookup x <$> ask >>= \ case
          Nothing -> throwError $ NotFound x <$ w
          Just x' -> return $ Var x'
      Arr a b -> Arr <$> rec a <*> rec b
