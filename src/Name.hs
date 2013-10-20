{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Name
       ( Name (..)
       , supplyName
       ) where

import Control.Applicative

import Data.Hashable (Hashable (hashWithSalt))

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

import Int
import Supply

data Name a = Name {-# UNPACK #-} !Int (Maybe a) deriving Show

instance Eq (Name a) where
  Name x _ == Name y _ = x == y
  Name x _ /= Name y _ = x /= y

instance Hashable (Name a) where
  hashWithSalt x = hashWithSalt x . toInt

instance IsInt (Name a) where
  toInt (Name x _) = x

instance Pretty a => Pretty (Name a) where
  pretty (Name x y) = maybe (char '$' <> pretty x) pretty y

instance PrettyTerm a => PrettyTerm (Name a) where
  prettyTerm (Name x y) = maybe (char '$' <> prettyTerm x) prettyTerm y

supplyName :: MonadSupply Int m => m (Name a)
supplyName = Name <$> supply <*> pure Nothing
