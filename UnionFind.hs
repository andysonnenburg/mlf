{-# LANGUAGE LambdaCase #-}
module UnionFind
       ( Set
       , new
       , union
       , find
       , write
       , Ref
       , read
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad
import Control.Monad.ST.Safe

import Data.Function (fix)
import Data.STRef
import Data.Semigroup

import Prelude hiding (elem, read)

import ST
import STIntRef

newtype Set s a = Set { unSet :: STRef s (Link s a) }

newtype Ref s a = Ref { unRef :: STRef s a } deriving Eq

data Link s a
  = Repr {-# UNPACK #-} !(STIntRef s) {-# UNPACK #-} !(STRef s a)
  | Link {-# UNPACK #-} !(STRef s (Link s a))

new :: MonadST m => a -> m (Set (World m) a)
new =
  liftST <<<
  fmap Set . newSTRef <=<
  liftA2 Repr (newSTIntRef minBound) <<<
  newSTRef

union :: (Semigroup a, MonadST m) => Set (World m) a -> Set (World m) a -> m ()
union x y = liftST $ do
  Three xRank xRef x' <- find' x
  Three yRank yRef y' <- find' y
  when (x' /= y') $
    compare <$> readSTIntRef xRank <*> readSTIntRef yRank >>= \ case
      LT -> do
        writeSTRef x' $ Link y'
        writeSTRef yRef =<< (<>) <$> readSTRef xRef <*> readSTRef yRef
      EQ -> do
        modifySTIntRef xRank (+ 1)
        writeSTRef y' $ Link x'
        writeSTRef xRef =<< (<>) <$> readSTRef xRef <*> readSTRef yRef
      GT -> do
        writeSTRef y' $ Link x'
        writeSTRef xRef =<< (<>) <$> readSTRef xRef <*> readSTRef yRef

find :: MonadST m => Set (World m) a -> m (Ref (World m) a)
find =
  liftST .
  fmap (\ (Two a _) -> Ref a) . fix (\ rec set -> readSTRef set >>= \ case
    Repr _ elem -> return $! Two elem set
    Link set' -> do
      two@(Two _ set'') <- rec set'
      writeSTRef set $ Link set''
      return two) . unSet

write :: MonadST m => Set (World m) a -> a -> m ()
{-# INLINE write #-}
write set a = liftST $ do
  ref <- find set
  writeSTRef (unRef ref) a

find' :: Set s a -> ST s (Three s a)
find' = fix (\ rec set -> readSTRef set >>= \ case
  Repr rank elem -> return $! Three rank elem set
  Link set' -> do
    three@(Three _ _ set'') <- rec set'
    writeSTRef set $ Link set''
    return three) . unSet

read :: MonadST m => Ref (World m) a -> m a
{-# INLINE read #-}
read = liftST . readSTRef . unRef

data Two s a =
  Two
  {-# UNPACK #-} !(STRef s a)
  {-# UNPACK #-} !(STRef s (Link s a))

data Three s a =
  Three
  {-# UNPACK #-} !(STIntRef s)
  {-# UNPACK #-} !(STRef s a)
  {-# UNPACK #-} !(STRef s (Link s a))
