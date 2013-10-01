{-# LANGUAGE
    DefaultSignatures
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
module Supply
       ( MonadSupply (..)
       , Supply
       , runSupply
       , SupplyT
       , runSupplyT
       ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Functor.Identity

import Hoist
import ST
import Stream

class (Applicative m, Monad m) => MonadSupply s m | m -> s where
  supply :: m s
  default supply :: (MonadTrans t, MonadSupply s m) => t m s
  supply = lift supply

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

instance MonadFix m => MonadFix (SupplyT s m) where
  mfix f = SupplyT $ mfix $ unSupplyT . f

instance MonadTrans (SupplyT s) where
  lift = SupplyT . lift

instance FunctorHoist (SupplyT s) where
  hoist f = SupplyT . hoist f . unSupplyT

instance (Applicative m, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    x :| xs <- get
    put xs
    return x

instance MonadError e m => MonadError e (SupplyT s m) where
  throwError = SupplyT . throwError
  m `catchError` h = SupplyT $ unSupplyT m `catchError` (unSupplyT . h)

instance MonadReader r m => MonadReader r (SupplyT s m) where
  ask = SupplyT ask
  local f = SupplyT . local f . unSupplyT

instance MonadST m => MonadST (SupplyT s m) where
  type World (SupplyT s m) = World m

instance MonadState s m => MonadState s (SupplyT s' m) where
  get = lift get
  put = lift . put

instance MonadSupply s m => MonadSupply s (ReaderT r m)
instance MonadSupply s m => MonadSupply s (StateT s' m)
