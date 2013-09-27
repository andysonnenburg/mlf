{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable #-}
module Id (Id (..)) where

import Control.Applicative
import Control.Comonad

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Text.PrettyPrint.Free

import Function

newtype Id a = Id { runId :: a } deriving ( Read
                                          , Show
                                          , Functor
                                          , Foldable
                                          , Traversable
                                          )

instance Comonad Id where
  {-# INLINE extract #-}
  extract = runId
  {-# INLINE duplicate #-}
  duplicate = Id

instance Applicative Id where
  {-# INLINE pure #-}
  pure = Id
  {-# INLINE (<*>) #-}
  f <*> a = Id $ runId f $ runId a

instance Monad Id where
  {-# INLINE return #-}
  return = Id
  {-# INLINE (>>=) #-}
  m >>= f = Id $ runId m |> runId . f

instance Pretty a => Pretty (Id a) where
  {-# INLINE pretty #-}
  pretty = pretty . runId
