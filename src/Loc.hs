{-# LANGUAGE
    DeriveGeneric
  , LambdaCase
  , MultiParamTypeClasses #-}
module Loc
       ( Pos (..)
       , plusPos
       , Loc (..)
       ) where

import Data.Semigroup

import GHC.Generics (Generic)

import Text.PrettyPrint.Free

import Lens

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving ( Show
                                                                , Eq
                                                                , Ord
                                                                , Generic
                                                                )

instance Field1 Pos Pos Int Int
instance Field2 Pos Pos Int Int

plusPos :: Char -> Pos -> Pos
plusPos = \ case
  '\t' -> lmap _2 (\ i -> (((i + 7) `div` 8) * 8 + 1))
  '\n' -> lmap _1 (+ 1)
  _ -> lmap _2 (+ 1)

instance Pretty Pos where
  pretty (Pos i j) = pretty i <> pretty ':' <> pretty j

data Loc = Loc {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos deriving (Show, Generic)

instance Field1 Loc Loc Pos Pos
instance Field2 Loc Loc Pos Pos

instance Semigroup Loc where
  Loc a b <> Loc a' b' = Loc (min a a') (max b b')

instance Pretty Loc where
  pretty (Loc x@(Pos i j) y@(Pos i' j')) = case (i == i', j == j') of
    (True, True) -> pretty x
    (True, False) -> pretty i <> char ':' <> pretty j <> char '-' <> pretty j'
    (False, _) -> pretty x <> char '-' <> pretty y
