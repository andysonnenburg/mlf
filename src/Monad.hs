{-# LANGUAGE LambdaCase #-}
module Monad
       ( whenM
       , unlessM
       , ifM
       ) where

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \ case
  True -> m
  False -> return ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= \ case
  True -> return ()
  False -> m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p m n = p >>= \ case
  True -> m
  False -> n
