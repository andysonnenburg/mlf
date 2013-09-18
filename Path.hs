{-# LANGUAGE LambdaCase #-}
module Path
       ( Path
       , empty
       , cons
       , length
       , toList
       , lca
       , keep
       , (~=)
       ) where

import Data.Function (fix)

import Prelude hiding (head, length)

infix 4 ~=

data Path
  = Nil
  | Cons
    {-# UNPACK #-} !Int
    {-# UNPAcK #-} !Int
    !Tree
    !Path

instance Show Path where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

data Tree
  = Tip {-# UNPACK #-} !Int
  | Bin {-# UNPACK #-} !Int !Tree !Tree

empty :: Path
{-# INLINE empty #-}
empty = Nil

cons :: Int -> Path -> Path
{-# INLINE cons #-}
cons x = \ case
  Cons n w t (Cons _ w' t' xs) | w == w' -> Cons (n + 1) (2 * w + 1) (Bin x t t') xs
  xs -> Cons (length xs + 1) 1 (Tip x) xs

length :: Path -> Int
{-# INLINE length #-}
length = \ case
  Nil -> 0
  Cons n _ _ _ -> n

toList :: Path -> [Int]
toList = \ case
  Nil -> []
  Cons _ _ t xs -> fix (\ rec -> \ case
    Tip x -> (x:)
    Bin x l r -> (x:) . rec l . rec r) t (toList xs)

lca :: Path -> Path -> Path
lca xs xs' = case compare n n' of
  LT -> dropUntilSame xs (keep n xs')
  EQ -> dropUntilSame xs xs'
  GT -> dropUntilSame (keep n' xs) xs'
  where
    n = length xs
    n' = length xs'

keep :: Int -> Path -> Path
keep = fix $ \ rec i -> \ case
  Nil -> Nil
  xs@(Cons n w t ys)
    | i >= n -> xs
    | otherwise -> case compare i (n - w) of
      LT -> rec i ys
      EQ -> xs
      GT -> go (i - n + w) w t ys
  where
    go n w (Bin _ l r) = case compare n w2 of
      LT -> go n w2 r
      EQ -> consTree w2 r
      GT | n == w - 1 -> consTree w2 l . consTree w2 r
         | otherwise -> go (n - w2) w2 l . consTree w2 r
      where
        w2 = w `div` 2
    go _ _ _ = id

(~=) :: Path -> Path -> Bool
{-# INLINE (~=) #-}
(~=) = sameHead

dropUntilSame :: Path -> Path -> Path
dropUntilSame xs@(Cons _ w t ys) (Cons _ _ t' ys')
  | sameRoot t t' = xs
  | sameHead ys ys' = go w t t' ys
  | otherwise = dropUntilSame ys ys'
  where
    go n (Bin _ l r) (Bin _ l' r')
      | sameRoot l l' = consTree n2 l . consTree n2 r
      | sameRoot r r' = go n2 l l' . consTree n2 r
      | otherwise = go n2 r r'
      where
        n2 = n `div` 2
    go _ _ _ = id
dropUntilSame _ _ = Nil

consTree :: Int -> Tree -> Path -> Path
{-# INLINE consTree #-}
consTree n t xs = Cons (n + length xs) n t xs

sameHead :: Path -> Path -> Bool
{-# INLINE sameHead #-}
sameHead Nil Nil = True
sameHead (Cons _ _ t _) (Cons _ _ t' _) = sameRoot t t'
sameHead _ _ = False

sameRoot :: Tree -> Tree -> Bool
{-# INLINE sameRoot #-}
sameRoot xs ys = root xs == root ys

root :: Tree -> Int
{-# INLINE root #-}
root = \ case
  Tip x -> x
  Bin x _ _ -> x
