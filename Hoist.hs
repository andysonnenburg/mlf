{-# LANGUAGE Rank2Types #-}
module Hoist (MonadHoist (..)) where

import Control.Monad.State.Strict

class MonadHoist t where
  hoist :: (forall a . m a -> n a) -> t m b -> t n b

instance MonadHoist (StateT s) where
  hoist f m = StateT $ \ s -> f (runStateT m s)
