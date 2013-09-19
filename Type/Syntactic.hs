{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleInstances
  , LambdaCase
  , OverloadedStrings #-}
module Type.Syntactic
       ( MonoType (..)
       , PolyType (..)
       , BindingFlag (..)
       ) where

import Control.Monad.Reader

import Data.Foldable
import Data.Text (Text)
import Data.Traversable

import Text.PrettyPrint.Free

import Name
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

instance Pretty (PolyType (Name Text)) where
  pretty = prettyPoly
    where
      prettyPoly = fix $ \ rec -> \ case
        Mono t -> prettyMono t
        Bot -> text "_|_"
        Forall x bf a b ->
          text "forall" <+> lparen <>
          prettyName x <+>
          prettyBindingFlag bf <+>
          rec a <> rparen <+>
          rec b
      prettyMono = fix $ \ rec -> \ case
        Var x -> prettyName x
        Arr a b -> rec a <+> text "->" <+> rec b
      prettyName = \ case
        Name Nothing x -> text "a" <> char '$' <> pretty x
        Name (Just a) x -> pretty a <> char '$' <> pretty x
      prettyBindingFlag = \ case
        Flexible -> char '>'
        Rigid -> char '='
