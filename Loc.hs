module Loc
       ( Pos (..)
       , plusPos
       , Loc (..)
       ) where

import Data.Semigroup

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Eq, Ord)

plusPos :: Char -> Pos -> Pos
plusPos x (Pos i j) = case x of
  '\t' -> Pos i (((j + 7) `div` 8) * 8 + 1)
  '\n' -> Pos (i + 1) 1
  _ -> Pos i (j + 1)

data Loc = Loc {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos deriving Show

instance Semigroup Loc where
  Loc a b <> Loc a' b' = Loc (min a a') (max b b')
