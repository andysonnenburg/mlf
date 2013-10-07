{-# LANGUAGE DefaultSignatures, TypeFamilies #-}
module ST (MonadST (..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.ST.Safe
import Control.Monad.State.Strict

class (Applicative m, Monad m) => MonadST m where
  type World m
  liftST :: ST (World m) a -> m a

  default liftST :: (MonadTrans t, MonadST m) => ST (World m) a -> t m a
  liftST = lift . liftST

instance MonadST (ST s) where
  type World (ST s) = s
  liftST = id

instance MonadST IO where
  type World IO = RealWorld
  liftST = stToIO

instance MonadST m => MonadST (ReaderT r m) where
  type World (ReaderT r m) = World m

instance MonadST m => MonadST (StateT s m) where
  type World (StateT s m) = World m
