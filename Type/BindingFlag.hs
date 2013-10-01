{-# LANGUAGE LambdaCase #-}
module Type.BindingFlag (BindingFlag (..)) where

import Data.Semigroup

import Text.PrettyPrint.Free

data BindingFlag = Flexible | Rigid deriving (Show, Eq)

instance Semigroup BindingFlag where
  Rigid <> _ = Rigid
  _ <> Rigid = Rigid
  Flexible <> Flexible = Flexible

instance Monoid BindingFlag where
  mempty = Flexible
  mappend = (<>)

instance Pretty BindingFlag where
  pretty = \ case
    Flexible -> char '>'
    Rigid -> char '='
