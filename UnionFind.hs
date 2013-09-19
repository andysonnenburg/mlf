{-# LANGUAGE LambdaCase #-}
module UnionFind
       ( Set
       , new
       , union
       , find
       , Ref
       , read
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe

import Data.Function (fix)
import Data.STRef
import Data.Semigroup

import Prelude hiding (elem, read)

import STIntRef

newtype Set s a = Set { unSet :: STRef s (Link s a) }

newtype Ref s a = Ref { unRef :: STRef s a } deriving Eq

data Link s a
  = RefLink {-# UNPACK #-} !(STIntRef s) {-# UNPACK #-} !(STRef s a)
  | SetLink {-# UNPACK #-} !(STRef s (Link s a))

new :: a -> ST s (Set s a)
new a =
  RefLink <$> newSTIntRef minBound <*> newSTRef a >>=
  fmap Set . newSTRef

union :: Semigroup a => Set s a -> Set s a -> ST s ()
union x y = do
  Three xRank xRef x' <- find' x
  Three yRank yRef y' <- find' y
  when (x' /= y') $
    compare <$> readSTIntRef xRank <*> readSTIntRef yRank >>= \ case
      LT -> do
        writeSTRef x' $ SetLink y'
        writeSTRef yRef =<< (<>) <$> readSTRef xRef <*> readSTRef yRef
      EQ -> do
        modifySTIntRef xRank (+ 1)
        writeSTRef y' $ SetLink x'
        writeSTRef xRef =<< (<>) <$> readSTRef xRef <*> readSTRef yRef
      GT -> do
        writeSTRef y' $ SetLink x'
        writeSTRef xRef =<< (<>) <$> readSTRef xRef <*> readSTRef yRef

find :: Set s a -> ST s (Ref s a)
find = fmap (\ (Two a _) -> Ref a) . fix (\ rec set -> readSTRef set >>= \ case
  RefLink _ elem -> return $! Two elem set
  SetLink set' -> do
    two@(Two _ set'') <- rec set'
    writeSTRef set $ SetLink set''
    return two) . unSet

find' :: Set s a -> ST s (Three s a)
find' = fix (\ rec set -> readSTRef set >>= \ case
  RefLink rank elem -> return $! Three rank elem set
  SetLink set' -> do
    three@(Three _ _ set'') <- rec set'
    writeSTRef set $ SetLink set''
    return three) . unSet

read :: Ref s a -> ST s a
{-# INLINE read #-}
read = readSTRef . unRef

data Two s a =
  Two
  {-# UNPACK #-} !(STRef s a)
  {-# UNPACK #-} !(STRef s (Link s a))

data Three s a =
  Three
  {-# UNPACK #-} !(STIntRef s)
  {-# UNPACK #-} !(STRef s a)
  {-# UNPACK #-} !(STRef s (Link s a))