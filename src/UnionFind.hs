{-# LANGUAGE
    DeriveGeneric
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses #-}
module UnionFind
       ( Set
       , new
       , union
       , unionWith
       , find
       , ref
       , Ref
       , read
       , write
       , contents
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Lens
import Control.Monad
import Control.Monad.ST.Safe

import Data.Function (fix)
import Data.STRef

import GHC.Generics (Generic)

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

union :: MonadST m => Set (World m) a -> Set (World m) a -> m ()
{-# INLINE union #-}
union = unionWith const

unionWith :: MonadST m
          => (a -> a -> a) -> Set (World m) a -> Set (World m) a -> m ()
unionWith f x y = liftST $ do
  Three xRankRef xRef xLinkRef <- find' x
  Three yRankRef yRef yLinkRef <- find' y
  when (xRef /= yRef) $
    compare <$> readSTIntRef xRankRef <*> readSTIntRef yRankRef >>= \ case
      LT -> do
        writeSTRef xLinkRef $ Link yLinkRef
        writeSTRef yRef =<< f <$> readSTRef xRef <*> readSTRef yRef
      EQ -> do
        modifySTIntRef xRankRef (+ 1)
        writeSTRef yLinkRef $ Link xLinkRef
        writeSTRef xRef =<< f <$> readSTRef xRef <*> readSTRef yRef
      GT -> do
        writeSTRef yLinkRef $ Link xLinkRef
        writeSTRef xRef =<< f <$> readSTRef xRef <*> readSTRef yRef

find :: MonadST m => Set (World m) a -> m (Ref (World m) a)
find = liftST . fmap (Ref . view _1) . fix (\ rec linkRef -> readSTRef linkRef >>= \ case
  Repr _ r -> return $! Two r linkRef
  Link linkRef' -> do
    x <- rec linkRef'
    writeSTRef linkRef $ Link $ x^._2
    return x) . unSet

ref :: MonadST m => IndexPreservingAction m (Set (World m) a) (Ref (World m) a)
ref = act find

write :: MonadST m => Ref (World m) a -> a -> m ()
{-# INLINE write #-}
write r = liftST . writeSTRef (unRef r)

read :: MonadST m => Ref (World m) a -> m a
{-# INLINE read #-}
read = liftST . readSTRef . unRef

contents :: MonadST m => IndexPreservingAction m (Ref (World m) a) a
contents = act read

find' :: Set s a -> ST s (Three s a)
find' = fix (\ rec linkRef -> readSTRef linkRef >>= \ case
  Repr rankRef r -> return $! Three rankRef r linkRef
  Link linkRef' -> do
    x <- rec linkRef'
    writeSTRef linkRef $ Link $ x^._3
    return x) . unSet

data Two s a =
  Two
  {-# UNPACK #-} !(STRef s a)
  {-# UNPACK #-} !(STRef s (Link s a)) deriving Generic

instance Field1 (Two s a) (Two s a) (STRef s a) (STRef s a)
instance Field2 (Two s a) (Two s a) (STRef s (Link s a)) (STRef s (Link s a))

data Three s a =
  Three
  {-# UNPACK #-} !(STIntRef s)
  {-# UNPACK #-} !(STRef s a)
  {-# UNPACK #-} !(STRef s (Link s a)) deriving Generic

instance Field1 (Three s a) (Three s a) (STIntRef s) (STIntRef s)
instance Field2 (Three s a) (Three s a) (STRef s a) (STRef s a)
instance Field3 (Three s a) (Three s a) (STRef s (Link s a)) (STRef s (Link s a))
