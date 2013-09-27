{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Name
       ( Name (..)
       , supplyName
       ) where

import Data.Functor
import Data.Hashable (Hashable (hashWithSalt))
import Data.Semigroup

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

import Int
import Supply

data Name a = Name (Maybe a) {-# UNPACK #-} !Int deriving Show

instance Eq (Name a) where
  Name _ x == Name _ y = x == y
  Name _ x /= Name _ y = x /= y

instance Semigroup (Name a) where
  n@(Name (Just _) _) <> _ = n
  Name Nothing x <> Name a _ = Name a x

instance Hashable (Name a) where
  hashWithSalt x (Name _ y) = hashWithSalt x y

instance IsInt (Name a) where
  toInt (Name _ x) = x

instance Pretty a => Pretty (Name a) where
  pretty (Name x y) = maybe (char '$' <> pretty y) pretty x

instance PrettyTerm a => PrettyTerm (Name a) where
  prettyTerm = \ case
    Name Nothing x -> char '$' <> prettyTerm x
    Name (Just a) _ -> prettyTerm a

supplyName :: MonadSupply Int m => m (Name a)
supplyName = Name Nothing <$> supply
