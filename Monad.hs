{-# LANGUAGE LambdaCase #-}
module Monad
       ( whenM
       ) where

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \ case
  True -> m
  False -> return ()
