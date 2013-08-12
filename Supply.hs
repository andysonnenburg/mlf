{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies 
  , MultiParamTypeClasses #-}
module Supply
       ( MonadSupply (..)
       , Supply
       , runSupply
       , SupplyT
       , runSupplyT
       ) where

import Control.Applicative
import Control.Monad.State.Strict

import Data.Functor.Identity

import Stream

class (Applicative m, Monad m) => MonadSupply s m | m -> s where
  supply :: m s

type Supply s = SupplyT s Identity

runSupply :: Supply s a -> Stream s -> a
runSupply m = runIdentity . runSupplyT m

newtype SupplyT s m a = SupplyT { unSupplyT :: StateT (Stream s) m a }

runSupplyT :: Monad m => SupplyT s m a -> Stream s -> m a
runSupplyT = evalStateT . unSupplyT

instance Functor m => Functor (SupplyT s m) where
  fmap f = SupplyT . fmap f . unSupplyT

instance (Applicative m, Monad m) => Applicative (SupplyT s m) where
  pure = SupplyT . pure
  f <*> a = SupplyT $ unSupplyT f <*> unSupplyT a

instance Monad m => Monad (SupplyT s m) where
  return = SupplyT . return
  m >>= k = SupplyT $ unSupplyT m >>= unSupplyT . k

instance (Applicative m, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    x :| xs <- get
    put xs
    return x
