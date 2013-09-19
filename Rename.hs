{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Rename (RenameError (..), rename) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map
import Data.Monoid

import Name
import Supply
import Type.Syntactic

data RenameError a = NotFound a deriving Show

rename :: ( Eq a
          , Hashable a
          , Applicative m
          , MonadError (RenameError a) m
          , MonadSupply Int m
          ) => PolyType a -> m (PolyType (Name a))
rename = flip runReaderT mempty . renamePoly
  where
    renamePoly = fix $ \ rec -> \ case
      Mono t -> Mono <$> renameMono t
      Bot -> return Bot
      Forall x bf a b -> do
        x' <- Name (Just x) <$> supply
        Forall x' bf <$> rec a <*> local (Map.insert x x') (rec b)
    renameMono = fix $ \ rec -> \ case
      Var x -> do
        Map.lookup x <$> ask >>= \ case
          Nothing -> throwError $ NotFound x
          Just x' -> return $ Var x'
      Arr a b -> Arr <$> rec a <*> rec b
