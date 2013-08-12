{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , StandaloneDeriving
  , UndecidableInstances #-}
module Loc
       ( Pos (..)
       , plusPos
       , Loc (..)
       , Located (..)
       , LocatedF (..)
       ) where

import Data.Foldable
import Data.Semigroup
import Data.Traversable

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Eq, Ord)

plusPos :: Char -> Pos -> Pos
plusPos x (Pos i j) = case x of
  '\t' -> Pos i (((j + 7) `div` 8) * 8 + 1)
  '\n' -> Pos (i + 1) 1
  _ -> Pos i (j + 1)

data Loc = Loc {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos deriving Show

instance Semigroup Loc where
  Loc a b <> Loc a' b' = Loc (min a a') (max b b')

data Located f a = Located {-# UNPACK #-} !Loc (f (Located f a))

deriving instance Show (f (Located f a)) => Show (Located f a)
deriving instance Functor f => Functor (Located f)
deriving instance Foldable f => Foldable (Located f)
deriving instance Traversable f => Traversable (Located f)

data LocatedF f a = LocatedF {-# UNPACK #-} !Loc (f (LocatedF f) a)

deriving instance Show (f (LocatedF f) a) => Show (LocatedF f a)
deriving instance Functor (f (LocatedF f)) => Functor (LocatedF f)
deriving instance Foldable (f (LocatedF f)) => Foldable (LocatedF f)
deriving instance Traversable (f (LocatedF f)) => Traversable (LocatedF f)
