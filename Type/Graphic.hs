module Type.Graphic
       ( Type
       , Term (..)
       , UnlabelledTerm (..)
       , Binders
       , BindingFlags
       , BindingFlag (..)
       ) where

import Data.IntMap.Strict (IntMap)
import Data.Semigroup

import Path
import Type.BindingFlag
import UnionFind

type Type s = (Term s, Binders, BindingFlags)

data Term s = Term {-# UNPACK #-} !Int (Set s (UnlabelledTerm s))

data UnlabelledTerm s
  = Bot
  | Arr (Term s) (Term s)

instance Semigroup (UnlabelledTerm s) where
  Bot <> a = a
  a <> _ = a

type Binders = IntMap Path

type BindingFlags = IntMap BindingFlag
