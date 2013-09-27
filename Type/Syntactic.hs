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
       , BindingFlag (..)
       ) where

import Control.Applicative
import Control.Comonad.Env (ComonadEnv)

import Data.Foldable
import Data.Function (fix)
import Data.Traversable

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

import Hoist
import Type.BindingFlag

data MonoType w a
  = Var a
  | Arr (w (MonoType w a)) (w (MonoType w a))
  deriving (Functor, Foldable, Traversable)
deriving instance ( Show a
                  , Show (w (MonoType w a))
                  ) => Show (MonoType w a)

instance ( Pretty a
         , Pretty (w (MonoType w a))
         ) => Pretty (MonoType w a) where
  pretty = \ case
    Var x -> pretty x
    Arr a b -> pretty a <+> text "->" <+> pretty b

instance ( PrettyTerm a
         , PrettyTerm (w (MonoType w a))
         ) => PrettyTerm (MonoType w a) where
  prettyTerm = \ case
    Var x -> prettyTerm x
    Arr a b -> prettyTerm a <+> text "->" <+> prettyTerm b

instance FunctorHoist MonoType where
  hoist f = fix $ \ rec -> \ case
    Var x -> Var x
    Arr a b -> Arr (f $ rec <$> a) (f $ rec <$> b)

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
         , Pretty (w (MonoType w a))
         , Pretty (w (PolyType w a))
         ) => Pretty (PolyType w a) where
  pretty = \ case
    Mono t -> pretty t
    Bot -> text "_|_"
    Forall x bf a b ->
      lparen <>
      pretty x <+>
      pretty bf <+>
      pretty a <>
      rparen <+>
      pretty b

instance ( ComonadEnv ScopedEffect w
         , PrettyTerm a
         , PrettyTerm (w (MonoType w a))
         , PrettyTerm (w (PolyType w a))
         ) => PrettyTerm (PolyType w a) where
  prettyTerm = \ case
    Mono t -> prettyTerm t
    Bot -> text "_|_"
    Forall x bf a b ->
      lparen <>
      prettyTerm x <+>
      pretty bf <+>
      prettyTerm a <>
      rparen <+>
      prettyTerm b

instance FunctorHoist PolyType where
  hoist f = fix $ \ rec -> \ case
    Mono t -> Mono $ f $ hoist f <$> t
    Bot -> Bot
    Forall a bf o o' -> Forall a bf (f $ rec <$> o) (f $ rec <$> o')
