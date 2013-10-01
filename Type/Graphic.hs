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
       , Binding (..)
       , bindingFlag
       , BindingFlag (..)
       , fromRestricted
       , toSyntactic
       , foldMapNode
       , forNode_
       , forNodeSet_
       ) where

import Control.Category ((<<<))
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.IntSet as Set
import Data.Maybe (fromMaybe)
import Data.Semigroup (Monoid (..), Semigroup ((<>)))

import Prelude hiding (read)

import Int
import IntMap (IntMap, (!))
import qualified IntMap as Map
import Name
import Product (Product (..))
import ST
import Supply
import Type.BindingFlag
import qualified Type.Restricted as R
import qualified Type.Syntactic as S
import UnionFind

type Type s a = NodeSet s a

type NodeSet s a = Set s (Node s a)

data Node s a = Node (Name a) (Binding s a) (Term s a)

instance Eq (Node s a) where
  Node a _ _ == Node b _ _ = a == b
  Node a _ _ /= Node b _ _ = a /= b

instance Semigroup (Node s a) where
  Node a b c <> Node a' b' c' = Node (a <> a') (b <> b') (c <> c')

instance Hashable (Node s a) where
  hashWithSalt x = hashWithSalt x . toInt

instance IsInt (Node s a) where
  toInt (Node a _ _) = toInt a

data Binding s a
  = Root
  | Binder BindingFlag (NodeSet s a)

instance Semigroup (Binding s a) where
  Root <> a = a
  a <> _ = a

instance Monoid (Binding s a) where
  mempty = Root
  mappend = (<>)

data Term s a
  = Bot
  | Arr (NodeSet s a) (NodeSet s a)

instance Semigroup (Term s a) where
  Bot <> a = a
  a <> _ = a

fromRestricted :: ( MonadST m
                  , MonadSupply Int m
                  ) => R.Type (Name a) -> m (Type (World m) a)
fromRestricted =
  flip runReaderT mempty <<<
  fix (\ rec name binding -> \ case
    R.Bot -> newNodeSet name binding Bot
    R.Var x -> asks (!x)
    R.Arr a b -> do
      p <- ask
      newNodeSet name binding $ Arr (p!a) (p!b)
    R.Forall x'@(Name name' _) bf o o' -> do
      n_a <- newNodeSet Nothing Root Bot
      t' <- local (Map.insert x' n_a) $ rec name binding o'
      sameNodeSet t' n_a >>= \ case
        True -> rec name' binding o
        False -> do
          t <- rec name' (Binder bf t') o
          union t n_a
          return t') Nothing Root
  where
    newNodeSet a b c = new =<< newNode a b c
    newNode a b c = Node <$> (Name a <$> supply) <*> pure b <*> pure c
    sameNodeSet t_a t_b = do
      r_a <- find t_a
      r_b <- find t_b
      if r_a == r_b
        then return True
        else do
        n_a <- read r_a
        n_b <- read r_b
        return $ n_a == n_b

toSyntactic :: MonadST m =>
               Type (World m) a ->
               m (Product Int (S.PolyType (Product Int) (Name a)))
toSyntactic t0 = do
  boundNodes <- getBoundNodes t0
  fix (\ rec n0@(Node (toInt -> x0) _ c) -> do
    t_s0 <- case c of
      Bot -> return $ x0 :* S.Bot
      Arr t_a t_b -> do
        Node x_a _ _ <- read =<< find t_a
        Node x_b _ _ <- read =<< find t_b
        return $ x0 :* S.Mono (S.Arr (toInt x_a :* S.Var x_a) (toInt x_b :* S.Var x_b))
    foldM (\ t_s n@(Node a@(toInt -> x) b _) -> do
      t_s' <- rec n
      return $ x :* S.Forall a (bindingFlag b) t_s' t_s)
      t_s0 (fromMaybe mempty $ Map.lookup n0 boundNodes)) =<< read =<< find t0

bindingFlag :: Binding s a -> BindingFlag
bindingFlag (Binder bf _) = bf
bindingFlag Root = error "bindingFlag: Root"

getBoundNodes :: (MonadST m, s ~ World m)
              => Type s a -> m (IntMap (Node s a) [Node s a])
getBoundNodes t0 = flip execStateT mempty $ forNode_ t0 $ \ n@(Node _ b _) ->
  case b of
    Root -> return ()
    Binder _ t' -> do
      n' <- read =<< find t'
      modify $ Map.alter (Just . maybe [n] (n:)) n'

foldMapNode :: ( MonadST m
               , s ~ World m
               , Monoid b
               ) => NodeSet s a -> (Node s a -> b) -> m b
foldMapNode t0 f = flip evalStateT mempty $ fix (\ rec t -> do
  n@(Node (toInt -> x) _ c) <- read =<< find t
  xs <- get
  if Set.notMember x xs
    then do
    modify $ Set.insert x
    case c of
      Bot -> return $ f n
      Arr a b -> do
        a' <- rec a
        b' <- rec b
        return $ a' `mappend` b' `mappend` f n
    else return mempty) t0

forNode_ :: ( MonadST m
            , s ~ World m
            ) => NodeSet s a -> (Node s a -> m b) -> m ()
forNode_ t0 f = forNodeSet_ t0 $ f <=< read <=< find

forNodeSet_ :: ( MonadST m
               , s ~ World m
               ) => NodeSet s a -> (NodeSet s a -> m b) -> m ()
forNodeSet_ t0 f = flip evalStateT mempty $ fix (\ rec t -> do
  Node (toInt -> x) _ c <- read =<< find t
  xs <- get
  when (Set.notMember x xs) $ do
    modify $ Set.insert x
    case c of
      Bot -> return ()
      Arr a b -> rec a >> rec b
    void $ lift $ f t) t0
