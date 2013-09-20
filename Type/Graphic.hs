{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , ViewPatterns #-}
module Type.Graphic
       ( Type
       , NodeSet
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

type Type s a = (NodeSet s a, Binders, BindingFlags)

type NodeSet s a = Set s (Node s a)

data Node s a = Node (Name a) (Term s a)

instance IsInt (Node s a) where
  toInt (Node x _) = toInt x

data Term s a
  = Bot
  | Arr (NodeSet s a) (NodeSet s a)

instance Semigroup (Term s a) where
  Bot <> a = a
  a <> _ = a

type Binders = IntMap Path

type BindingFlags = IntMap BindingFlag

fromRestricted :: ( MonadST m
                  , MonadSupply Int m
                  ) => R.Type (Name a) -> m (Type (World m) a)
fromRestricted =
  liftST . prune <=<
  run <<<
  fix (\ rec h -> \ case
    R.Bot -> newSet h Bot
    R.Var (toInt -> x) -> (!x) <$> ask
    R.Arr a b -> do
      p <- ask
      newSet h $ Arr (p!toInt a) (p!toInt b)
    R.Forall (Name h' x) bf o o' -> do
      t <- rec h' o
      t' <- local (Map.insert x t) $ rec h o'
      sameSet t t' >>= \ case
        True -> return t
        False -> do
          tellBinding t bf t'
          return t') Nothing
  where
    tellBinding t bf t' = do
      r_t <- liftST $ fmap toInt . read =<< find t
      r_t' <- liftST $ fmap toInt . read =<< find t'
      (t_b, t_bf) <- get
      let b = Path.cons r_t' $ findBinder r_t' t_b
      put (Map.insert r_t b t_b, Map.insert r_t bf t_bf)
    newSet x t = liftST . new =<< newNode x t
    newNode x t = Node <$> (Name x <$> supply) <*> pure t
    sameSet = liftA2 (==) `on` liftST . find
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
  fix (\ rec (Node (toInt -> x) c) -> do
    t0 <- case c of
      Bot -> return S.Bot
      Arr t t' -> do
        Node a _ <- read =<< find t
        Node a' _ <- read =<< find t'
        return $ S.Mono $ S.Arr (S.Var a) (S.Var a')
    foldM (\ t n@(Node a _) -> do
      t' <- rec n
      return $ S.Forall a (t_bf!toInt a) t' t)
      t0
      (fromMaybe [] $ Map.lookup x bound)) =<< read =<< find t_n

forNode_ :: MonadST m => NodeSet (World m) a -> (Node (World m) a -> m b) -> m ()
forNode_ t_n0 f = flip evalStateT mempty $ fix (\ rec t_n -> do
  n@(Node (toInt -> x) c) <- liftST $ read =<< find t_n
  xs <- get
  when (Set.notMember x xs) $ do
    modify (Set.insert x)
    case c of
      Bot -> return ()
      Arr a b -> rec a >> rec b
    void $ lift $ f n) t_n0

findBinder :: Int -> Binders -> Path
findBinder x xs = case Map.lookup x xs of
  Nothing -> Path.empty
  Just y -> y
