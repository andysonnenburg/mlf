{-# LANGUAGE
    DeriveGeneric
  , LambdaCase
  , MultiParamTypeClasses #-}
module Type.BindingFlag
       ( BindingFlag (..)
       , flexible
       , rigid
       ) where

import Control.Lens

import Data.Semigroup

import GHC.Generics (Generic)

import Text.PrettyPrint.Free (Pretty (pretty), char)

data BindingFlag = Flexible | Rigid deriving (Show, Eq, Generic)

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

instance VariantA BindingFlag BindingFlag () ()
instance VariantB BindingFlag BindingFlag () ()

flexible :: Prism' BindingFlag ()
flexible = _A

rigid :: Prism' BindingFlag ()
rigid = _B
