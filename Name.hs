{-# LANGUAGE FlexibleContexts #-}
module Name
       ( Name (..)
       , supplyName
       ) where

import Data.Functor

import Supply

data Name a = Name (Maybe a) {-# UNPACK #-} !Int deriving Show

supplyName :: MonadSupply Int m => m (Name a)
supplyName = Name Nothing <$> supply
