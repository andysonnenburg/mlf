module Loc
       ( Pos (..)
       , plusPos
       , Loc (..)
       ) where

import Data.Semigroup

import Text.PrettyPrint.Free

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Eq, Ord)

plusPos :: Char -> Pos -> Pos
plusPos x (Pos i j) = case x of
  '\t' -> Pos i (((j + 7) `div` 8) * 8 + 1)
  '\n' -> Pos (i + 1) 1
  _ -> Pos i (j + 1)

instance Pretty Pos where
  pretty (Pos i j) = pretty i <> pretty ':' <> pretty j

data Loc = Loc {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos deriving Show

instance Semigroup Loc where
  Loc a b <> Loc a' b' = Loc (min a a') (max b b')

instance Pretty Loc where
  pretty (Loc x@(Pos i j) y@(Pos i' j')) = case (i == i', j == j') of
    (True, True) -> pretty x
    (True, False) -> pretty i <> char ':' <> pretty j <> char '-' <> pretty j'
    (False, _) -> pretty x <> char '-' <> pretty y
