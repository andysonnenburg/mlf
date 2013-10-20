{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , LambdaCase
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances #-}
module Catch
       ( MonadCatch (..)
       , mapE
       ) where

import Control.Monad.Error

class (MonadError e m, Monad n) => MonadCatch e m n | n e -> m where
  catch :: m a -> (e -> n a) -> n a

mapE :: (MonadCatch e m n, MonadError e' n) => (e -> e') -> m a -> n a
mapE f m = m `catch` (throwError . f)

instance Error e => MonadCatch e (Either e) (Either e') where
  Left e `catch` h = h e
  Right a `catch` _h = Right a
