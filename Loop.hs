{-# LANGUAGE
    DeriveFunctor
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
module Loop (Loop, loop, LoopT, loopT, recur) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Data.Functor.Identity

import ST
import Supply

type Loop a b = LoopT a b Identity

loop :: a -> (a -> Loop a b b) -> b
{-# INLINE loop #-}
loop a = runIdentity . loopT a

newtype LoopT a b m c = LoopT { unLoopT :: ReaderT (a -> m b) m c
                              } deriving Functor

instance Applicative m => Applicative (LoopT a b m) where
  {-# INLINE pure #-}
  pure = LoopT . pure
  {-# INLINE (<*>) #-}
  f <*> a = LoopT $ unLoopT f <*> unLoopT a

instance Monad m => Monad (LoopT a b m) where
  {-# INLINE return #-}
  return = LoopT . return
  {-# INLINE (>>=) #-}
  m >>= k = LoopT $ unLoopT m >>= unLoopT . k
  {-# INLINE fail #-}
  fail = LoopT . fail

instance MonadTrans (LoopT a b) where
  {-# INLINE lift #-}
  lift = LoopT . lift

loopT :: a -> (a -> LoopT a b m b) -> m b
{-# INLINE loopT #-}
loopT a0 k = runReaderT (unLoopT $ k a0) $ fix $ \ rec a -> runReaderT (unLoopT $ k a) rec

recur :: Monad m => a -> LoopT a b m b
{-# INLINE recur #-}
recur a = LoopT $ ask >>= lift . ($ a)

instance MonadError e m => MonadError e (LoopT a b m) where
  throwError = LoopT . throwError
  m `catchError` h = LoopT $ unLoopT m `catchError` (unLoopT . h)

instance MonadReader r m => MonadReader r (LoopT a b m) where
  ask = LoopT $ lift ask
  local f = LoopT . mapReaderT (local f) . unLoopT

instance MonadST m => MonadST (LoopT a b m) where
  type World (LoopT a b m) = World m

instance MonadSupply s m => MonadSupply s (LoopT a b m)
