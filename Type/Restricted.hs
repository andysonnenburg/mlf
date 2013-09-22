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

import Control.Category ((>>>))
import Control.Comonad

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

fromSyntactic :: ( Comonad w
                 , MonadSupply Int m
                 ) => w (PolyType w (Name a)) -> m (Type (Name a))
fromSyntactic = fromPoly
  where
    fromPoly = fix $ \ rec -> extract >>> \ case
      S.Mono t -> fromMono t
      S.Bot -> return Bot
      S.Forall x bf a b -> do
        a' <- rec a
        b' <- rec b
        return $ Forall x bf a' b'
    fromMono = fix $ \ rec -> extract >>> \ case
      S.Var x -> return $ Var x
      S.Arr t u -> do
        t' <- rec t
        a <- supplyName
        u' <- rec u
        b <- supplyName
        return $ Forall a Flexible t' $ Forall b Flexible u' $ Arr a b
