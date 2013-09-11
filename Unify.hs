{-# LANGUAGE LambdaCase #-}
module Unify (unify) where

import Control.Applicative
import Control.Monad.ST.Safe
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Function (fix)
import Data.IntMap.Strict (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Semigroup

import Path (Path)
import Type.Graphic
import UnionFind

unify :: Type s -> [Term s] -> MaybeT (ST s) (Type s)
unify (t, t_b, t_bf) n = do
  p <- getPermissions t t_b t_bf
  S bots merged <- execStateT (traversePairs_ unify' n) initS
  let t_bf' = bindingFlags t_bf merged
  grafted <- getGrafted t bots
  let t_b' = binders t_b merged grafted
  traverseWithKey_ (\ i x ->
    unlessGreen (p!i) $
      unlessBotM (lift (readSet x)) $
        empty) bots
  traverseTerm_ (\ i ->
    whenRed (p!i) $
      unless (sameHead (t_b!i) (t_b'!i)) $
        empty) t
  traverseTerm_ (\ i ->
    whenRed (p!i) $
      unless (t_bf!i == t_bf'!i) $
        empty) t
  return (t, t_b', t_bf')

unify' :: Term s -> Term s -> Unify s ()
unify' (Term i1 x1) (Term i2 x2) =
  (,) <$> readSet' x1 <*> readSet' x2 >>= \ case
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

getGrafted :: Term s -> IntMap a -> Rebind s (IntMap Int)
getGrafted = undefined

binders :: Binders -> IntMap IntSet -> IntMap Int -> Binders
binders = undefined

type Unify s = StateT (S s) (MaybeT (ST s))

data S s = S (IntMap (Set s (UnlabelledTerm s))) (IntMap IntSet)

initS :: S s
initS = undefined

markBot :: Int -> Unify s ()
markBot = undefined

markMerged :: Int -> Int -> Unify s ()
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

getPermissions :: Term s -> Binders -> BindingFlags -> Rebind s Permissions
getPermissions = undefined

traverseTerm_ :: (Int -> Rebind s a) -> Term s -> Rebind s ()
traverseTerm_ = undefined

unlessBotM :: Monad m => m (UnlabelledTerm s) -> m () -> m ()
unlessBotM m n = m >>= \ case
  Bot -> return ()
  _ -> n

readSet' :: Set s a -> Unify s a
readSet' = lift . lift . readSet

union' :: Semigroup a => Set s a -> Set s a -> Unify s ()
union' x y = lift $ lift $ union x y

sameHead :: Path -> Path -> Bool
sameHead = undefined

traverseWithKey_ :: (Int -> a -> m b) -> IntMap a -> m ()
traverseWithKey_ = undefined

traversePairs_ :: Applicative f => (a -> a -> f ()) -> [a] -> f ()
traversePairs_ f = \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs
