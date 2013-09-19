{-# LANGUAGE FlexibleContexts #-}
module Name
       ( Name (..)
       , supplyName
       ) where

import Data.Functor
import Data.Hashable (Hashable (hashWithSalt))

import Int
import Supply

data Name a = Name (Maybe a) {-# UNPACK #-} !Int deriving Show

instance Eq (Name a) where
  Name _ x == Name _ y = x == y
  Name _ x /= Name _ y = x /= y

instance Hashable (Name a) where
  hashWithSalt x (Name _ y) = hashWithSalt x y

instance IsInt (Name a) where
  toInt (Name _ x) = x

supplyName :: MonadSupply Int m => m (Name a)
supplyName = Name Nothing <$> supply