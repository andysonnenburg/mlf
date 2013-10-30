{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , TypeFamilies #-}
module Type.Graphic
       ( module Type.Node
       , Type
       , BoundNode
       , Bound (..)
       , Term (..)
       , Binding (..)
       , BindingFlag (..)
       , fromRestricted
       , toSyntactic
       ) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Lens
import Control.Monad.Reader

import Data.Foldable (Foldable (foldMap), foldlM, foldrM)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)

import GHC.Generics (Generic)

import Prelude hiding (read)

import Function
import Int
import IntMap (IntMap, (!))
import qualified IntMap as Map
import Name
import Product (Product (..))
import ST
import Supply
import Type.BindingFlag
import Type.Node
import qualified Type.Restricted as R
import qualified Type.Syntactic as S
import UnionFind

type Type s a = Set s (BoundNode s a)

type BoundNode s a = Node s (Bound s a)

data Bound s a b =
  Bound a {-# UNPACK #-} !(Set s (Binding b)) !(Term b) deriving Generic

instance Field1 (Bound s a b) (Bound s a' b) a a'
instance Field2 (Bound s a b) (Bound s a b) (Set s (Binding b)) (Set s (Binding b))
instance Field3 (Bound s a b) (Bound s a b) (Term b) (Term b)

instance Foldable (Bound s a) where
  foldMap f = foldMap f . view _3

data Binding a
  = Root
  | Binder !BindingFlag a deriving ( Show
                                   , Functor
                                   , Foldable
                                   , Traversable
                                   )

data Term a
  = Bot
  | Arr a a deriving ( Show
                     , Functor
                     , Foldable
                     , Traversable
                     )

fromRestricted :: (MonadST m, MonadSupply Int m)
               => R.Type (Name a) -> m (Type (World m) (Maybe a))
fromRestricted =
  flip runReaderT mempty <<<
  fix (\ rec name binding -> \ case
    R.Bot -> newBoundNodeSet name binding Bot
    R.Var x -> asks (!x)
    R.Arr a b -> do
      p <- ask
      newBoundNodeSet name binding $ Arr (p!a) (p!b)
    R.Forall x'@(Name _ name') bf o o' -> do
      n_a <- newBoundNodeSet Nothing Root Bot
      t' <- local (Map.insert x' n_a) $ rec name binding o'
      same t' n_a >>= \ case
        True -> rec name' binding o
        False -> do
          t <- rec name' (Binder bf t') o
          union t n_a
          return t') Nothing Root
  where
    newBoundNodeSet a b c = new =<< newBoundNode a b c
    newBoundNode a b c = Bound a <$> new b <*> pure c >>= newNode
    same s_a s_b = do
      r_a <- find s_a
      r_b <- find s_b
      if r_a == r_b
        then return True
        else do
        a <- read r_a
        b <- read r_b
        return $ a == b

toSyntactic :: MonadST m
            => Type (World m) (Maybe a)
            -> m (Product Int (S.PolyType (Product Int) (Name a)))
toSyntactic t0 = do
  bns <- getBoundNodes t0
  fix (\ rec n0 -> do
    t_s0 <- case n0^.projected._3 of
      Bot -> return $ toInt n0 :* S.Bot
      Arr t_a t_b -> do
        n_a <- read =<< find t_a
        n_b <- read =<< find t_b
        return $ toInt n0 :* S.Mono (S.Arr (nodeVar n_a) (nodeVar n_b))
    foldrM (\ (bf, n) t_s -> nodeForall n bf <$> rec n <*> pure t_s)
      t_s0 (fromMaybe mempty $ Map.lookup n0 bns)) =<< read =<< find t0
  where
    nodeVar n = toInt n :* S.Var (nodeName n)
    nodeForall n bf o o' = toInt n :* S.Forall (nodeName n) bf o o'
    nodeName n = Name (n^.int) (n^.projected._1)

getBoundNodes :: (MonadST m, s ~ World m)
              => Type s a
              -> m (IntMap (BoundNode s a) [(BindingFlag, BoundNode s a)])
getBoundNodes = find >=> read >=> preorder >=> foldlM (\ ns' ->
  find >=> read >=> \ n -> n^.projected._2 $$ find >=> read >=> \ case
    Root -> return ns'
    Binder bf s' -> find s' >>= read <&> \ n' -> Map.alter (\ case
      Nothing -> Just [(bf, n)]
      Just ns -> Just ((bf, n):ns)) n' ns') mempty
