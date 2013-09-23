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

import Control.Comonad.Env (ComonadEnv)

import Data.Foldable
import Data.Function (fix)
import Data.Traversable

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

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

instance ( Pretty a
         , Pretty (w (MonoType w (Name a)))
         ) => Pretty (MonoType w (Name a)) where
  pretty = \ case
    Var x -> prettyName x
    Arr a b -> pretty a <+> text "->" <+> pretty b

instance ( PrettyTerm a
         , PrettyTerm (w (MonoType w (Name a)))
         ) => PrettyTerm (MonoType w (Name a)) where
  prettyTerm = \ case
    Var x -> prettyTermName x
    Arr a b -> prettyTerm a <+> text "->" <+> prettyTerm b

data PolyType w a
  = Mono (w (MonoType w a))
  | Bot
  | Forall a BindingFlag (w (PolyType w a)) (w (PolyType w a))
  deriving (Functor, Foldable, Traversable)
deriving instance ( Show a
                  , Show (w (MonoType w a))
                  , Show (w (PolyType w a))
                  ) => Show (PolyType w a)

instance ( Pretty a
         , Pretty (w (MonoType w (Name a)))
         , Pretty (w (PolyType w (Name a)))
         ) => Pretty (PolyType w (Name a)) where
  pretty = \ case
    Mono t -> pretty t
    Bot -> text "_|_"
    Forall x bf a b ->
      text "forall" <+>
      lparen <>
      prettyName x <+>
      pretty bf <+>
      pretty a <>
      rparen <+>
      pretty b

prettyName :: Pretty a => Name a -> Doc e
prettyName = \ case
  Name Nothing x -> char '$' <> pretty x
  Name (Just a) _ -> pretty a

instance ( ComonadEnv ScopedEffect w
         , PrettyTerm a
         , PrettyTerm (w (MonoType w (Name a)))
         , PrettyTerm (w (PolyType w (Name a)))
         ) => PrettyTerm (PolyType w (Name a)) where
  prettyTerm = \ case
    Mono t -> prettyTerm t
    Bot -> text "_|_"
    Forall x bf a b ->
      text "forall" <+>
      lparen <>
      prettyTermName x <+>
      pretty bf <+>
      prettyTerm a <>
      rparen <+>
      prettyTerm b

prettyTermName :: PrettyTerm a => Name a -> TermDoc
prettyTermName = \ case
  Name Nothing x -> char '$' <> prettyTerm x
  Name (Just a) _ -> prettyTerm a

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
