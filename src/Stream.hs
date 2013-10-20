{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable #-}
module Stream
       ( Stream (..)
       , enumFrom
       ) where

import Data.Foldable
import Data.Traversable

import Prelude hiding (enumFrom)

infixr 5 :|

data Stream a = a :| Stream a deriving (Show, Functor, Foldable, Traversable)

enumFrom :: Enum a => a -> Stream a
enumFrom a = a :| enumFrom (succ a)
