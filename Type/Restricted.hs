{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , LambdaCase #-}
module Type.Restricted
       ( Type (..)
       , BindingFlag (..)
       ) where

import Data.Foldable
import Data.Traversable

import Type.BindingFlag

data Type a
  = Var a
  | Arr (Type a) (Type a)
  | Bot
  | Forall a BindingFlag (Type a) (Type a)
  deriving (Show, Functor, Foldable, Traversable)
