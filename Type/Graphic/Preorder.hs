{-# LANGUAGE TypeFamilies, ViewPatterns #-}
module Type.Graphic.Preorder
       ( foldl'
       , foldlM
       , foldlSM
       , for_
       ) where

import Control.Monad.State.Strict

import qualified Data.IntSet as Set
import Data.Monoid (mempty)

import Prelude hiding (read)

import Int
import ST
import Type.Graphic
import UnionFind

foldl' :: (MonadST m, s ~ World m)
       => (b -> Node s a -> b) -> b -> NodeSet s a -> m b
foldl' f b0 t0 = flip evalStateT mempty $ fix (\ rec b t -> do
  n@(Node (toInt -> x) _ c) <- read =<< find t
  xs <- get
  if Set.member x xs
    then return b
    else do
    modify $ Set.insert x
    case c of
      Bot -> return $ b `seq` f b n
      Arr t_a t_b -> do
        let b' = b `seq` f b n
        b'' <- rec b' t_a
        rec b'' t_b) b0 t0

foldlM :: (MonadST m, s ~ World m)
       => (b -> Node s a -> m b) -> b -> NodeSet s a -> m b
foldlM f b0 t0 = flip evalStateT mempty $ fix (\ rec b t -> do
  n@(Node (toInt -> x) _ c) <- read =<< find t
  xs <- get
  if Set.member x xs
    then return b
    else do
    modify $ Set.insert x
    case c of
      Bot -> lift $ f b n
      Arr t_a t_b -> do
        b' <- lift $ f b n
        b'' <- rec b' t_a
        rec b'' t_b) b0 t0

foldlSM :: (MonadST m, s ~ World m)
       => (b -> NodeSet s a -> m b) -> b -> NodeSet s a -> m b
foldlSM f b0 t0 = flip evalStateT mempty $ fix (\ rec b t -> do
  Node (toInt -> x) _ c <- read =<< find t
  xs <- get
  if Set.member x xs
    then return b
    else do
    modify $ Set.insert x
    case c of
      Bot -> lift $ f b t
      Arr t_a t_b -> do
        b' <- lift $ f b t
        b'' <- rec b' t_a
        rec b'' t_b) b0 t0

for_ :: (MonadST m, s ~ World m) => NodeSet s a -> (Node s a -> m b) -> m ()
for_ t0 f = flip evalStateT mempty $ fix (\ rec t -> do
  n@(Node (toInt -> x) _ c) <- read =<< find t
  xs <- get
  when (Set.notMember x xs) $ do
    modify $ Set.insert x
    _ <- lift $ f n
    case c of
      Bot -> return ()
      Arr t_a t_b -> rec t_a >> rec t_b) t0
