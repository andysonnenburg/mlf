{-# LANGUAGE LambdaCase #-}
module Type.Graphic
       ( Type
       , Node (..)
       , Term (..)
       , Binders
       , BindingFlags
       , BindingFlag (..)
       , fromRestricted
       , toSyntactic
       ) where

import Control.Monad.ST.Safe

import Data.IntMap.Strict (IntMap)
import Data.Semigroup

import Name
import Path
import Type.BindingFlag
import qualified Type.Restricted as R
import qualified Type.Syntactic as S
import UnionFind

type Type s a = (Node s a, Binders, BindingFlags)

data Node s a = Node (Name a) (Set s (Term s a))

data Term s a
  = Bot
  | Arr (Node s a) (Node s a)

instance Semigroup (Term s a) where
  Bot <> a = a
  a <> _ = a

type Binders = IntMap Path

type BindingFlags = IntMap BindingFlag

fromRestricted :: R.Type a -> ST s (Type s a)
fromRestricted = undefined

toSyntactic :: Type s a -> ST s (S.PolyType a)
toSyntactic = undefined
