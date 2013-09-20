{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Unify (unify) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST.Safe
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.IntMap.Strict (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Semigroup

import Prelude hiding (read)

import Name
import Path ((~=))
import ST
import Type.Graphic
import UnionFind

data UnifyError s a
  = Name a `OccursIn` Term s a
  | Term s a `DoesNotMatch` Term s a

unify :: ( MonadError (UnifyError (World m) a) m
         , MonadST m
         ) => Type (World m) a -> [NodeSet (World m) a] -> m (Type (World m) a)
unify (t, t_b, t_bf) n = do
  p <- getPermissions t t_b t_bf
  S bots merged <- execStateT (traversePairs_ unify' n) initS
  let t_bf' = bindingFlags t_bf merged
  grafted <- getGrafted t bots
  let t_b' = binders t_b merged grafted
  traverseWithKey_ (\ i x ->
    unlessGreen (p!i) $
      unlessBotM (lift (read =<< find x)) $
        empty) bots
  traverseNode_ (\ i ->
    whenRed (p!i) $
      unless (t_b!i ~= t_b'!i) $
        empty) t
  traverseNode_ (\ i ->
    whenRed (p!i) $
      unless (t_bf!i == t_bf'!i) $
        empty) t
  return (t, t_b', t_bf')

unify' :: Node s a -> Node s a -> Unify a s ()
unify' (Node i1 x1) (Node i2 x2) = do
  r1 <- find' x1
  r2 <- find' x2
  when (r1 /= r2) $ (,) <$> read' r1 <*> read' r2 >>= \ case
    (Bot, Bot) -> do
      markBot i1
      markBot i2
      markMerged i1 i2
      union' x1 x2
    (Bot, _) -> do
      markBot i1
      markMerged i1 i2
      union' x1 x2
    (_, Bot) -> do
      markBot i2
      markMerged i1 i2
      union' x1 x2
    (Arr a1 b1, Arr a2 b2) -> do
      markMerged i1 i2
      unify' a1 a2
      unify' b1 b2

bindingFlags :: BindingFlags -> IntMap IntSet -> BindingFlags
bindingFlags = undefined

getGrafted :: Node s a -> IntMap b -> Rebind s (IntMap Int)
getGrafted = undefined

binders :: Binders -> IntMap IntSet -> IntMap Int -> Binders
binders = undefined

type Unify n s = StateT (S n s) (MaybeT (ST s))

data S n s = S (IntMap (Set s (Term s n))) (IntMap IntSet)

initS :: S n s
initS = undefined

markBot :: Name a -> Unify a s ()
markBot = undefined

markMerged :: Name a -> Name a -> Unify a s ()
markMerged = undefined

type Rebind s = MaybeT (ST s)

type Permissions = IntMap Permission

data Permission = M | I | G | O | R deriving Show

unlessGreen :: Monad m => Permission -> m () -> m ()
unlessGreen G _ = return ()
unlessGreen _ m = m

whenRed :: Monad m => Permission -> m () -> m ()
whenRed R m = m
whenRed _ _ = return ()

getPermissions :: Node s a -> Binders -> BindingFlags -> Rebind s Permissions
getPermissions = undefined

traverseNode_ :: (Int -> Rebind s a) -> Node s n -> Rebind s ()
traverseNode_ = undefined

unlessBotM :: Monad m => m (Term s a) -> m () -> m ()
unlessBotM m n = m >>= \ case
  Bot -> return ()
  _ -> n

find' :: Set s a -> Unify n s (Ref s a)
find' = lift . lift . find

read' :: Ref s a -> Unify n s a
read' = lift . lift . read

union' :: Semigroup a => Set s a -> Set s a -> Unify n s ()
union' x y = lift $ lift $ union x y

traverseWithKey_ :: (Int -> a -> m b) -> IntMap a -> m ()
traverseWithKey_ = undefined

traversePairs_ :: Applicative f => (a -> a -> f ()) -> [a] -> f ()
traversePairs_ f = \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs
