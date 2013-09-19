{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , ViewPatterns #-}
module Type.Graphic
       ( Type
       , Node (..)
       , Term (..)
       , Binders
       , BindingFlags
       , BindingFlag (..)
       , fromRestricted
       , toSyntactic
       , forNode_
       , findBinder
       ) where

import Control.Category ((<<<))
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.ST.Safe
import Control.Monad.State.Strict

import Data.Function (on)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Maybe (fromMaybe)
import Data.Semigroup

import Prelude hiding (read)

import Int
import Name
import Path
import ST
import Supply
import Type.BindingFlag
import qualified Type.Restricted as R
import qualified Type.Syntactic as S
import UnionFind

type Type s a = (Node s a, Binders, BindingFlags)

data Node s a = Node (Name a) (Set s (Term s a))

instance IsInt (Node s a) where
  toInt (Node x _) = toInt x

data Term s a
  = Bot
  | Arr (Node s a) (Node s a)

instance Semigroup (Term s a) where
  Bot <> a = a
  a <> _ = a

type Binders = IntMap Path

type BindingFlags = IntMap BindingFlag

fromRestricted :: ( MonadSupply Int m
                  , MonadST m
                  ) => R.Type (Name a) -> m (Type (World m) a)
fromRestricted =
  liftST . prune <=<
  run <<<
  fix (\ rec -> \ case
    R.Bot -> newNode Nothing Bot
    R.Var (toInt -> x) -> (!x) <$> ask
    R.Arr a b -> do
      p <- ask
      Node <$> supplyName <*> liftST (newSet (Arr (p!toInt a) (p!toInt b)))
    R.Forall a bf o o' -> do
      t <- rec o
      t' <- local (Map.insert (toInt a) t) $ rec o'
      sameSet t t' >>= \ case
        True -> return t
        False -> do
          tellBinding (toInt t) bf (toInt t')
          return t')
  where
    tellBinding r_t bf r_t' = do
      (t_b, t_bf) <- get
      put (Map.insert r_t (Path.cons r_t' $ findBinder r_t' t_b) t_b,
           Map.insert r_t bf t_bf)
    newNode x t = Node <$> (Name x <$> supply) <*> liftST (newSet t)
    newSet = liftST . new
    sameSet = liftA2 (==) `on` liftST . find . nodeSet
    nodeSet (Node _ x) = x
    run =
      fmap (\ (a, (b, c)) -> (a, b, c)) .
      flip runStateT mempty .
      flip runReaderT mempty

prune :: Type s a -> ST s (Type s a)
prune (t, t_b, t_bf) = do
  reachable <- execStateT (forNode_ t $ modify . flip Map.insert () . toInt) mempty
  return (t, Map.intersection t_b reachable, Map.intersection t_bf reachable)

toSyntactic :: Type s a -> ST s (S.PolyType (Name a))
toSyntactic (t_n, t_b, t_bf) = do
  bound <- flip execStateT mempty $ forNode_ t_n $ \ n@(Node (toInt -> x) _) ->
    case Path.uncons =<< Map.lookup x t_b of
      Nothing -> return ()
      Just (y, _) -> modify $ Map.alter (\ case
        Nothing -> Just [n]
        Just ns -> Just (n:ns)) y
  fix (\ rec (Node (toInt -> x) y) -> do
    t0 <- fmap
          (\ case
              Bot -> S.Bot
              Arr (Node a _) (Node b _) -> S.Mono $ S.Arr (S.Var a) (S.Var b)) .
          read =<<
          find y
    foldM (\ t n@(Node a _) -> do
      t' <- rec n
      return $ S.Forall a (t_bf!toInt a) t' t)
      t0
      (fromMaybe [] $ Map.lookup x bound)) t_n

forNode_ :: MonadST m => Node (World m) a -> (Node (World m) a -> m b) -> m ()
forNode_ n0 f = flip evalStateT mempty $ fix (\ rec n@(Node (toInt -> x) y) ->
  gets (Set.member x) >>= \ case
    True -> return ()
    False -> do
      modify (Set.insert x)
      liftST (find y >>= read) >>= \ case
        Bot -> return ()
        Arr a b -> rec a >> rec b
      void $ lift $ f n) n0

findBinder :: Int -> Binders -> Path
findBinder x xs = case Map.lookup x xs of
  Nothing -> Path.empty
  Just y -> y
