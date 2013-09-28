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

import Control.Applicative
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
      S.Forall x bf a b -> Forall x bf <$> rec a <*> rec b
    fromMono = fix $ \ rec -> extract >>> \ case
      S.Var x -> return $ Var x
      S.Arr t u -> do
        a <- supplyName
        b <- supplyName
        Forall a Flexible <$> rec t <*> (Forall b Flexible <$> rec u <*> pure (Arr a b))
