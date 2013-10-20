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

import Text.PrettyPrint.Free (Pretty (pretty), char)

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
  '\t' -> lmap _2 ((+ 1) . (* 8) . (`div` 8) . (+ 7))
  '\n' -> lmap _1 (+ 1)
  _ -> lmap _2 (+ 1)

instance Pretty Pos where
  pretty x = pretty (x^._1) <> pretty ':' <> pretty (x^._2)

data Loc = Loc {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos deriving (Show, Generic)

instance Field1 Loc Loc Pos Pos
instance Field2 Loc Loc Pos Pos

instance Semigroup Loc where
  x <> y = Loc (min (x^._1) (y^._1)) (max (x^._2) (y^._2))

instance Pretty Loc where
  pretty (Loc x@(Pos i j) y@(Pos i' j')) = case (i == i', j == j') of
    (True, True) -> pretty x
    (True, False) -> pretty i <> char ':' <> pretty j <> char '-' <> pretty j'
    (False, _) -> pretty x <> char '-' <> pretty y
