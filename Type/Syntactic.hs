{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable #-}
module Type.Syntactic
       ( MonoType (..)
       , PolyType (..)
       , BindingType (..)
       ) where

import Data.Foldable
import Data.Traversable

import Type.Common

data MonoType a
  = Var a
  | MonoType a :-> MonoType a
  deriving (Show, Functor, Foldable, Traversable)

data PolyType a
  = MonoType (MonoType a)
  | Bottom
  | Forall a BindingType (PolyType a) (PolyType a)
  deriving (Show, Functor, Foldable, Traversable)
