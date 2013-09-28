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
       , Binders
       , BindingFlags
       , BindingFlag (..)
       , fromRestricted
       , toSyntactic
       , forNode_
       , forNodeSet_
       , forNode_'
       ) where

import Control.Category ((<<<))
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Function (on)
import Data.Hashable (Hashable (hashWithSalt))
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup ((<>)), mempty)

import Prelude hiding (read)

import Int
import Name
import Product (Product (..))
import ST
import Supply
import Type.BindingFlag
import qualified Type.Restricted as R
import qualified Type.Syntactic as S
import UnionFind

type Type s a = (NodeSet s a, Binders, BindingFlags)

type NodeSet s a = Set s (Node s a)

data Node s a = Node (Name a) (Term s a)

instance Semigroup (Node s a) where
  Node a c <> Node a' c' = Node (a <> a') (c <> c')

instance Hashable (Node s a) where
  hashWithSalt x = hashWithSalt x . toInt

instance IsInt (Node s a) where
  toInt (Node a _) = toInt a

data Term s a
  = Bot
  | Arr (NodeSet s a) (NodeSet s a)

instance Semigroup (Term s a) where
  Bot <> a = a
  a <> _ = a

type Binders = IntMap Int

type BindingFlags = IntMap BindingFlag

fromRestricted :: ( MonadST m
                  , MonadSupply Int m
                  ) => R.Type (Name a) -> m (Type (World m) a)
fromRestricted =
  prune <=<
  run <<<
  fix (\ rec name -> \ case
    R.Bot -> newNodeSet name Bot
    R.Var (toInt -> x) -> (!x) <$> ask
    R.Arr a b -> do
      p <- ask
      newNodeSet name $ Arr (p!toInt a) (p!toInt b)
    R.Forall (Name name' x) bf o o' -> do
      t <- rec name' o
      n_a <- newNodeSet Nothing Bot
      t' <- local (Map.insert x n_a) $ rec name o'
      sameSet t' n_a >>= \ case
        True -> return t
        False -> do
          union t n_a
          tellBinding t bf t'
          return t') Nothing
  where
    tellBinding t bf t' = do
      r_t <- fmap toInt . read =<< find t
      r_t' <- fmap toInt . read =<< find t'
      (t_b, t_bf) <- get
      put (Map.insert r_t r_t' t_b, Map.insert r_t bf t_bf)
    newNodeSet x c = new =<< newNode x c
    newNode x c = Node <$> (Name x <$> supply) <*> pure c
    sameSet = liftA2 (==) `on` find
    run =
      fmap (\ (a, (b, c)) -> (a, b, c)) .
      flip runStateT mempty .
      flip runReaderT mempty

prune :: MonadST m => Type (World m) a -> m (Type (World m) a)
prune (t, t_b, t_bf) = do
  xs <- execStateT (forNode_ t $ modify . flip Map.insert () . toInt) mempty
  return (t, Map.intersection t_b xs, Map.intersection t_bf xs)

toSyntactic :: MonadST m =>
               Type (World m) a ->
               m (Product Int (S.PolyType (Product Int) (Name a)))
toSyntactic (t_n, t_b, t_bf) = do
  boundNodes <- getBoundNodes t_n t_b
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
      t0 (fromMaybe [] $ Map.lookup x0 boundNodes)) =<< read =<< find t_n

getBoundNodes :: ( MonadST m
                 , s ~ World m
                 ) => NodeSet s a -> Binders -> m (IntMap [Node s a])
getBoundNodes t_n t_b =
  flip execStateT mempty $ forNode_ t_n $ \ n@(Node (toInt -> x) _) ->
  case Map.lookup x t_b of
    Nothing -> return ()
    Just y -> modify $ Map.alter (Just . maybe [n] (n:)) y

forNode_ :: ( MonadST m
            , s ~ World m
            ) => NodeSet s a -> (Node s a -> m b) -> m ()
forNode_ t_n0 f = forNodeSet_ t_n0 $ f <=< read <=< find

forNodeSet_ :: ( MonadST m
               , s ~ World m
               ) => NodeSet s a -> (NodeSet s a -> m b) -> m ()
forNodeSet_ t_n0 f = flip evalStateT mempty $ fix (\ rec t_n -> do
  Node (toInt -> x) c <- read =<< find t_n
  xs <- get
  when (Set.notMember x xs) $ do
    modify $ Set.insert x
    case c of
      Bot -> return ()
      Arr a b -> rec a >> rec b
    void $ lift $ f t_n) t_n0

forNode_' :: ( MonadST m
             , s ~ World m
             ) => NodeSet s a -> (Node s a -> m b) -> m ()
forNode_' t_n0 f = flip evalStateT mempty $ fix (\ rec t_n -> do
  n@(Node (toInt -> x) c) <- read =<< find t_n
  xs <- get
  when (Set.notMember x xs) $ do
    modify $ Set.insert x
    _ <- lift $ f n
    case c of
      Bot -> return ()
      Arr a b -> rec a >> rec b) t_n0
