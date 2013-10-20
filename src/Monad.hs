{-# LANGUAGE LambdaCase #-}
module Monad
       ( whenM
       , unlessM
       ) where

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \ case
  True -> m
  False -> return ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= \ case
  True -> return ()
  False -> m
