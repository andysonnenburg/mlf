{-# LANGUAGE Rank2Types #-}
module Hoist
       ( FunctorHoist (..)
       , hoist'
       ) where

import Control.Monad.State.Strict

class FunctorHoist t where
  hoist :: (Functor f, Functor g) => (forall a . f a -> g a) -> t f b -> t g b

instance FunctorHoist (StateT s) where
  hoist f m = StateT $ \ s -> f (runStateT m s)

hoist' :: ( Functor f
          , Functor g
          , FunctorHoist t
          ) => (forall a . f a -> g a) -> f (t f b) -> g (t g b)
hoist' f = f . fmap (hoist f)
