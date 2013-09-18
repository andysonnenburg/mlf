{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable #-}
module Type.Syntactic
       ( MonoType (..)
       , PolyType (..)
       , BindingFlag (..)
       ) where

import Data.Foldable
import Data.Traversable

import Type.BindingFlag

data MonoType a
  = Var a
  | Arr (MonoType a) (MonoType a)
  deriving (Show, Functor, Foldable, Traversable)

data PolyType a
  = Mono (MonoType a)
  | Bot
  | Forall a BindingFlag (PolyType a) (PolyType a)
  deriving (Show, Functor, Foldable, Traversable)
