{-# LANGUAGE LambdaCase #-}
module Path
       ( Path
       , empty
       , cons
       , uncons
       , length
       , toList
       , lca
       , keep
       , (~=)
       ) where

import Data.Function (fix)

import Prelude hiding (head, length)

infix 4 ~=

data Path a
  = Nil
  | Cons
    {-# UNPACK #-} !Int
    {-# UNPAcK #-} !Int
    !(Tree a)
    !(Path a)

instance Show a => Show (Path a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

data Tree a
  = Tip {-# UNPACK #-} !Int !a
  | Bin {-# UNPACK #-} !Int !a !(Tree a) !(Tree a)

empty :: Path a
{-# INLINE empty #-}
empty = Nil

cons :: Int -> a -> Path a -> Path a
{-# INLINE cons #-}
cons k v = \ case
  Cons n w t (Cons _ w' t' xs) | w == w' -> Cons (n + 1) (2 * w + 1) (Bin k v t t') xs
  xs -> Cons (length xs + 1) 1 (Tip k v) xs

uncons :: Path a -> Maybe (Int, a, Path a)
uncons = \ case
  Nil -> Nothing
  Cons _ _ (Tip k v) xs -> Just (k, v, xs)
  Cons _ w (Bin k v l r) xs -> Just (k, v, consTree w2 l (consTree w2 r xs))
    where
      w2 = w `div` 2

length :: Path a -> Int
{-# INLINE length #-}
length = \ case
  Nil -> 0
  Cons n _ _ _ -> n

toList :: Path a -> [(Int, a)]
toList = \ case
  Nil -> []
  Cons _ _ t xs -> fix (\ rec -> \ case
    Tip k v -> ((k, v):)
    Bin k v l r -> ((k, v):) . rec l . rec r) t (toList xs)

lca :: Path a -> Path a -> Path a
lca xs xs' = case compare n n' of
  LT -> dropUntilSame xs (keep n xs')
  EQ -> dropUntilSame xs xs'
  GT -> dropUntilSame (keep n' xs) xs'
  where
    n = length xs
    n' = length xs'

keep :: Int -> Path a -> Path a
keep = fix $ \ rec i -> \ case
  Nil -> Nil
  xs@(Cons n w t ys)
    | i >= n -> xs
    | otherwise -> case compare i (n - w) of
      LT -> rec i ys
      EQ -> ys
      GT -> go (i - n + w) w t ys
  where
    go n w (Bin _ _ l r) = case compare n w2 of
      LT -> go n w2 r
      EQ -> consTree w2 r
      GT | n == w - 1 -> consTree w2 l . consTree w2 r
         | otherwise -> go (n - w2) w2 l . consTree w2 r
      where
        w2 = w `div` 2
    go _ _ _ = id

(~=) :: Path a -> Path b -> Bool
{-# INLINE (~=) #-}
(~=) = sameHead

dropUntilSame :: Path a -> Path b -> Path a
dropUntilSame xs@(Cons _ w t ys) (Cons _ _ t' ys')
  | sameRoot t t' = xs
  | sameHead ys ys' = go w t t' ys
  | otherwise = dropUntilSame ys ys'
  where
    go n (Bin _ _ l r) (Bin _ _ l' r')
      | sameRoot l l' = consTree n2 l . consTree n2 r
      | sameRoot r r' = go n2 l l' . consTree n2 r
      | otherwise = go n2 r r'
      where
        n2 = n `div` 2
    go _ _ _ = id
dropUntilSame _ _ = Nil

consTree :: Int -> Tree a -> Path a -> Path a
{-# INLINE consTree #-}
consTree n t xs = Cons (n + length xs) n t xs

sameHead :: Path a -> Path b -> Bool
{-# INLINE sameHead #-}
sameHead Nil Nil = True
sameHead (Cons _ _ t _) (Cons _ _ t' _) = sameRoot t t'
sameHead _ _ = False

sameRoot :: Tree a -> Tree b -> Bool
{-# INLINE sameRoot #-}
sameRoot xs ys = root xs == root ys

root :: Tree a -> Int
{-# INLINE root #-}
root = \ case
  Tip k _ -> k
  Bin k _ _ _ -> k
