{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , LambdaCase #-}
module Type.Restricted
       ( Type (..)
       , BindingType (..)
       , fromSyntactic
       ) where

import Control.Applicative

import Data.Foldable
import Data.Function
import Data.Traversable

import Supply
import Type.Common
import qualified Type.Syntactic as Syntactic

data Type a
  = Var a
  | a :-> a
  | Bottom
  | Forall a BindingType (Type a) (Type a)
  deriving (Show, Functor, Foldable, Traversable)

fromSyntactic :: MonadSupply a m => Syntactic.PolyType a -> m (Type a)
fromSyntactic = fix $ \ rec -> \ case
  Syntactic.MonoType t -> fix (\ rec' -> \ case
    Syntactic.Var a -> return $ Var a
    t1 Syntactic.:-> t2 ->
      supply >>= \ a1 ->
      supply >>= \ a2 ->
      Forall a1 Flexible <$>
      rec' t1 <*>
      (Forall a2 Flexible <$> rec' t2 <*> pure (a1 :-> a2))) t
  Syntactic.Bottom -> return Bottom
  Syntactic.Forall a b o o' -> Forall a b <$> rec o <*> rec o'
