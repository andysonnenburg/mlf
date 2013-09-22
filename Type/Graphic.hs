{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , TypeFamilies
  , ViewPatterns #-}
module Type.Graphic
       ( Type
       , NodeSet
       , Node (..)
       , Term (..)
       , Paths
       , BindingFlags
       , BindingFlag (..)
       , fromRestricted
       , toSyntactic
       , getBoundNodes
       , forNode_
       , forNode_'
       , findPath
       ) where

import Control.Category ((<<<))
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Function (on)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup ((<>)), mempty)

import Prelude hiding (read)

import Int
import Name
import Path
import Product (Product (..))
import ST
import Supply
import Type.BindingFlag
import qualified Type.Restricted as R
import qualified Type.Syntactic as S
import UnionFind

type Type s a = (NodeSet s a, Paths, BindingFlags)

type NodeSet s a = Set s (Node s a)

data Node s a = Node (Name a) (Term s a)

instance Semigroup (Node s a) where
  Node a c <> Node a' c' = Node (a <> a') (c <> c')

instance IsInt (Node s a) where
  toInt (Node a _) = toInt a

data Term s a
  = Bot
  | Arr (NodeSet s a) (NodeSet s a)

instance Semigroup (Term s a) where
  Bot <> a = a
  a <> _ = a

type Paths = IntMap Path

type BindingFlags = IntMap BindingFlag

fromRestricted :: ( MonadST m
                  , MonadSupply Int m
                  ) => R.Type (Name a) -> m (Type (World m) a)
fromRestricted =
  prune <=<
  run <<<
  fix (\ rec h -> \ case
    R.Bot -> newNodeSet h Bot
    R.Var (toInt -> x) -> (!x) <$> ask
    R.Arr a b -> do
      p <- ask
      newNodeSet h $ Arr (p!toInt a) (p!toInt b)
    R.Forall (Name h' x) bf o o' -> do
      t <- rec h' o
      n_a <- newNodeSet Nothing Bot
      t' <- local (Map.insert x n_a) $ rec h o'
      sameSet t' n_a >>= \ case
        True -> return t
        False -> do
          liftST $ union t n_a
          tellBinding t bf t'
          return t') Nothing
  where
    tellBinding t bf t' = do
      r_t <- liftST $ fmap toInt . read =<< find t
      r_t' <- liftST $ fmap toInt . read =<< find t'
      (t_p, t_bf) <- get
      let p = Path.cons r_t' $ findPath r_t' t_p
      put (Map.insert r_t p t_p, Map.insert r_t bf t_bf)
    newNodeSet x c = liftST . new =<< newNode x c
    newNode x c = Node <$> (Name x <$> supply) <*> pure c
    sameSet = liftA2 (==) `on` liftST . find
    run =
      fmap (\ (a, (b, c)) -> (a, b, c)) .
      flip runStateT mempty .
      flip runReaderT mempty

prune :: MonadST m => Type (World m) a -> m (Type (World m) a)
prune (t, t_p, t_bf) = do
  xs <- execStateT (forNode_ t $ modify . flip Map.insert () . toInt) mempty
  return (t, Map.intersection t_p xs, Map.intersection t_bf xs)

toSyntactic :: MonadST m =>
               Type (World m) a ->
               m (Product Int (S.PolyType (Product Int) (Name a)))
toSyntactic (t_n, t_p, t_bf) = liftST $ do
  bs <- getBoundNodes t_n t_p
  fix (\ rec (Node (toInt -> x0) c) -> do
    t0 <- case c of
      Bot -> return $ x0 :* S.Bot
      Arr t t' -> do
        Node a _ <- read =<< find t
        Node a' _ <- read =<< find t'
        return $ x0 :* S.Mono (x0 :* S.Arr (toInt a :* S.Var a) (toInt a' :* S.Var a'))
    foldM (\ t n@(Node a@(toInt -> x) _) -> do
      t' <- rec n
      return $ x :* S.Forall a (t_bf!x) t' t)
      t0 (fromMaybe [] $ Map.lookup x0 bs)) =<< read =<< find t_n

getBoundNodes :: ( MonadST m
                 , s ~ World m
                 ) => NodeSet s a -> Paths -> m (IntMap [Node s a])
getBoundNodes t_n t_p =
  flip execStateT mempty $ forNode_ t_n $ \ n@(Node (toInt -> x) _) ->
  case Path.uncons =<< Map.lookup x t_p of
    Nothing -> return ()
    Just (y, _) -> modify $ Map.alter (Just . maybe [n] (n:)) y

forNode_ :: ( MonadST m
            , s ~ World m
            ) => NodeSet s a -> (Node s a -> m b) -> m ()
forNode_ t_n0 f = flip evalStateT mempty $ fix (\ rec t_n -> do
  n@(Node (toInt -> x) c) <- liftST $ read =<< find t_n
  xs <- get
  when (Set.notMember x xs) $ do
    modify $ Set.insert x
    case c of
      Bot -> return ()
      Arr a b -> rec a >> rec b
    void $ lift $ f n) t_n0

forNode_' :: ( MonadST m
             , s ~ World m
             ) => NodeSet s a -> (Node s a -> m b) -> m ()
forNode_' t_n0 f = flip evalStateT mempty $ fix (\ rec t_n -> do
  n@(Node (toInt -> x) c) <- liftST $ read =<< find t_n
  xs <- get
  when (Set.notMember x xs) $ do
    modify $ Set.insert x
    _ <- lift $ f n
    case c of
      Bot -> return ()
      Arr a b -> rec a >> rec b) t_n0

findPath :: Int -> Paths -> Path
findPath x ps = case Map.lookup x ps of
  Nothing -> Path.empty
  Just p -> p
