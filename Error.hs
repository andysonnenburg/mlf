{-# LANGUAGE
    FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
module Error (ErrorT (..)) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans.Class

import ST
import Supply

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance Functor m => Functor (ErrorT e m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Applicative m, Monad m) => Applicative (ErrorT e m) where
  pure = ErrorT . pure . Right
  f <*> a = ErrorT $ runErrorT f >>= \ case
    Left e -> pure (Left e)
    Right f' -> runErrorT a >>= \ case
      Left e -> pure (Left e)
      Right a' -> pure (Right (f' a'))

instance Monad m => Monad (ErrorT e m) where
  return = ErrorT . return . Right
  m >>= f = ErrorT $ runErrorT m >>= \ case
    Left e -> return (Left e)
    Right a -> runErrorT (f a)

instance MonadTrans (ErrorT e) where
  lift = ErrorT . liftM Right

instance Monad m => MonadError e (ErrorT e m) where
  throwError = ErrorT . return . Left
  m `catchError` h = ErrorT $ runErrorT m >>= \ case
    Left e -> runErrorT (h e)
    Right a -> return (Right a)

instance MonadST m => MonadST (ErrorT e m) where
  type World (ErrorT e m) = World m
instance MonadSupply s m => MonadSupply s (ErrorT e m)
