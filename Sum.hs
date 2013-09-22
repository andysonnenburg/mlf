{-# LANGUAGE
    DeriveFunctor
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
module Sum
       ( Sum (..)
       , SumT (..)
       ) where

import Control.Applicative
import Control.Monad.Error

import Catch
import ST
import Supply

data Sum a b = L a | R b deriving ( Eq
                                  , Ord
                                  , Read
                                  , Show
                                  , Functor
                                  )

instance Applicative (Sum e) where
  pure = R
  L e <*> _ = L e
  R f <*> a = fmap f a

instance Monad (Sum e) where
  return = R
  L e >>= _ = L e
  R a >>= f = f a

instance MonadError e (Sum e) where
  throwError = L
  L e `catchError` h = h e
  s@(R _) `catchError` _ = s

instance MonadCatch e (Sum e) (Sum e') where
  L e `catch` h = h e
  R a `catch` _ = R a

newtype SumT e m a = SumT { runSumT :: m (Sum e a) } deriving Functor

instance (Applicative m, Monad m) => Applicative (SumT e m) where
  pure = SumT . pure . R
  f <*> a = SumT $ runSumT f >>= \ case
    L e -> pure (L e)
    R f' -> runSumT a >>= \ case
      L e -> pure (L e)
      R a' -> pure (R (f' a'))

instance Monad m => Monad (SumT e m) where
  return = SumT . return . R
  m >>= f = SumT $ runSumT m >>= \ case
    L e -> return (L e)
    R a -> runSumT (f a)

instance MonadTrans (SumT e) where
  lift = SumT . liftM R

instance Monad m => MonadError e (SumT e m) where
  throwError = SumT . return . L
  m `catchError` h = SumT $ runSumT m >>= \ case
    L e -> runSumT (h e)
    R a -> return (R a)

instance Monad m => MonadCatch e (SumT e m) (SumT e' m) where
  m `catch` h = SumT $ runSumT m >>= \ case
    L e -> runSumT $ h e
    R a -> return $ R a

instance MonadST m => MonadST (SumT e m) where
  type World (SumT e m) = World m

instance MonadSupply s m => MonadSupply s (SumT e m)
