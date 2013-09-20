{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , LambdaCase #-}
module Type.Restricted
       ( Type (..)
       , BindingFlag (..)
       , fromSyntactic
       ) where

import Data.Foldable
import Data.Function (fix)
import Data.Traversable

import Name
import Type.BindingFlag
import Type.Syntactic (PolyType)
import qualified Type.Syntactic as S
import Supply

data Type a
  = Var a
  | Arr a a
  | Bot
  | Forall a BindingFlag (Type a) (Type a)
  deriving (Show, Functor, Foldable, Traversable)

fromSyntactic :: MonadSupply Int m => PolyType (Name a) -> m (Type (Name a))
fromSyntactic = fromPoly
  where
    fromPoly = fix $ \ rec -> \ case
      S.Mono t -> fromMono t
      S.Bot -> return Bot
      S.Forall x bf a b -> do
        a' <- rec a
        b' <- rec b
        return $ Forall x bf a' b'
    fromMono = fix $ \ rec -> \ case
      S.Var x -> return $ Var x
      S.Arr t u -> do
        t' <- rec t
        a <- supplyName
        u' <- rec u
        b <- supplyName
        return $ Forall a Flexible t' $ Forall b Flexible u' $ Arr a b
