{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , Rank2Types
  , StandaloneDeriving
  , UndecidableInstances
  , ViewPatterns #-}
module Type.Syntactic
       ( MonoType (..)
       , PolyType (..)
       , mapEnv
       , BindingFlag (..)
       ) where

import Control.Category ((>>>))
import Control.Comonad.Env (Comonad, ComonadEnv, ask, extract)

import Data.Foldable
import Data.Function (fix)
import Data.Traversable

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

import Function
import Name
import Product
import Type.BindingFlag

data MonoType w a
  = Var a
  | Arr (w (MonoType w a)) (w (MonoType w a))
  deriving (Functor, Foldable, Traversable)
deriving instance ( Show a
                  , Show (w (MonoType w a))
                  ) => Show (MonoType w a)

data PolyType w a
  = Mono (w (MonoType w a))
  | Bot
  | Forall a BindingFlag (w (PolyType w a)) (w (PolyType w a))
  deriving (Functor, Foldable, Traversable)
deriving instance ( Show a
                  , Show (w (MonoType w a))
                  , Show (w (PolyType w a))
                  ) => Show (PolyType w a)

mapEnv :: (a -> b) ->
          Product a (PolyType (Product a) c) ->
          Product b (PolyType (Product b) c)
mapEnv f = mapPoly
  where
    mapPoly = fix $ \ rec -> local f . fmap (\ case
      Mono w -> Mono $ mapMono w
      Bot -> Bot
      Forall a bf w w' -> Forall a bf (rec w) (rec w'))
    mapMono = fix $ \ rec -> local f . fmap (\ case
      Var a -> Var a
      Arr w w' -> Arr (rec w) (rec w'))

instance (Comonad w, Pretty a) => Pretty (PolyType w (Name a)) where
  pretty = prettyPoly
    where
      prettyPoly = fix $ \ rec -> \ case
        Mono t -> prettyMono t
        Bot -> text "_|_"
        Forall x bf (extract -> a) (extract -> b) ->
          text "forall" <+>
          lparen <>
          prettyName x <+>
          pretty bf <+>
          rec a <>
          rparen <+>
          rec b
      prettyMono = fix $ \ rec -> extract >>> \ case
        Var x -> prettyName x
        Arr a b -> rec a <+> text "->" <+> rec b
      prettyName = \ case
        Name Nothing x -> char '$' <> pretty x
        Name (Just a) _ -> pretty a

instance ( ComonadEnv ScopedEffect w
         , PrettyTerm a
         ) => PrettyTerm (PolyType w (Name a)) where
  prettyTerm = prettyPoly
    where
      prettyPoly = fix $ \ rec -> \ case
        Mono t -> prettyMono t
        Bot -> text "_|_"
        Forall x bf a b ->
          text "forall" <+>
          lparen <>
          prettyName x <+>
          pretty bf <+>
          with (ask a) (rec $ extract a) <>
          rparen <+>
          with (ask b) (rec $ extract b)
      prettyMono = fix $ \ rec w -> with (ask w) $ extract w |> \ case
        Var x -> prettyName x
        Arr a b -> rec a <+> text "->" <+> rec b
      prettyName = \ case
        Name Nothing x -> char '$' <> prettyTerm x
        Name (Just a) _ -> prettyTerm a
