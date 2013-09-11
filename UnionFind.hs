{-# LANGUAGE LambdaCase #-}
module UnionFind
       ( Set
       , new
       , union
       , find
       , Elem
       , read
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe

import Data.Function (fix)
import Data.Semigroup
import Data.STRef

import Prelude hiding (elem, read)

import STIntRef

newtype Set s a = Set { unSet :: STRef s (Link s a) }

newtype Elem s a = Elem { unElem :: STRef s a } deriving Eq

data Link s a
  = ElemLink {-# UNPACK #-} !(STIntRef s) {-# UNPACK #-} !(Elem s a)
  | SetLink {-# UNPACK #-} !(Set s a)

new :: a -> ST s (Set s a)
new a =
  ElemLink <$> newSTIntRef minBound <*> (Elem <$> newSTRef a) >>=
  fmap Set . newSTRef

union :: Semigroup a => Set s a -> Set s a -> ST s ()
union x y = do
  Triple xRank xElem x' <- find' x
  Triple yRank yElem y' <- find' y
  when (unSet x' /= unSet y') $
    compare <$> readSTIntRef xRank <*> readSTIntRef yRank >>= \ case
      LT -> do
        writeSet x' $ SetLink y'
        write yElem =<< (<>) <$> read xElem <*> read yElem
      EQ -> do
        incrementRank xRank
        writeSet y' $ SetLink x'
        write xElem =<< (<>) <$> read xElem <*> read yElem
      GT -> do
        writeSet y' $ SetLink x'
        write xElem =<< (<>) <$> read xElem <*> read yElem

find :: Set s a -> ST s (Elem s a)
find = fmap fst' . fix (\ rec set -> readSet set >>= \ case
  ElemLink _ elem -> return $ elem :* set
  SetLink set' -> do
    r@(_ :* set'') <- rec set'
    writeSet set $ SetLink set''
    return r)

find' :: Set s a -> ST s (Triple s a)
find' = fix $ \ rec set -> readSet set >>= \ case
  ElemLink rank elem -> return $ Triple rank elem set
  SetLink set' -> do
    r@(Triple _ _ set'') <- rec set'
    writeSet set $ SetLink set''
    return r

read :: Elem s a -> ST s a
{-# INLINE read #-}
read = readSTRef . unElem

data Pair s a = {-# UNPACK #-} !(Elem s a) :* !(Set s a)

fst' :: Pair s a -> Elem s a
{-# INLINE fst' #-}
fst' (a :* _) = a

data Triple s a =
  Triple
  {-# UNPACK #-} !(STIntRef s)
  {-# UNPACK #-} !(Elem s a)
  {-# UNPACK #-} !(Set s a)

readSet :: Set s a -> ST s (Link s a)
{-# INLINE readSet #-}
readSet = readSTRef . unSet

writeSet :: Set s a -> Link s a -> ST s ()
{-# INLINE writeSet #-}
writeSet = writeSTRef . unSet

write :: Elem s a -> a -> ST s ()
{-# INLINE write #-}
write = writeSTRef . unElem

incrementRank :: STIntRef s -> ST s ()
{-# INLINE incrementRank #-}
incrementRank ref = readSTIntRef ref >>= writeSTIntRef ref . (+ 1)
