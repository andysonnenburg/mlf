{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Stream
       ( Stream (..)
       , enumFrom
       ) where

import Control.Comonad

import Data.Foldable
import Data.Traversable

import GHC.Generics (Generic)

import Prelude hiding (enumFrom)

import Lens

infixr 5 :|

data Stream a = a :| Stream a deriving ( Show
                                       , Functor
                                       , Foldable
                                       , Traversable
                                       , Generic
                                       )

instance Field1 (Stream a) (Stream a) a a
instance Field2 (Stream a) (Stream a) (Stream a) (Stream a)

instance Comonad Stream where
  extract = lask _1
  duplicate x = x :| duplicate (x^._2)

enumFrom :: Enum a => a -> Stream a
enumFrom a = a :| enumFrom (succ a)
