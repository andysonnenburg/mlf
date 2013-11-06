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
       ( module Type.BindingFlag
       , module Type.Node
       , Type
       , BoundNode
       , Bound (..)
       , binding
       , term
       , Binding (..)
       , root
       , binder
       , Term (..)
       , bot
       , arr
       , fromRestricted
       , toSyntactic
       , syntactic
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

import Int
import IntMap (IntMap, (!))
import Monad
import Name
import Product (Product (..))
import ST
import Supply
import Type.BindingFlag
import Type.Node
import qualified Type.Restricted as R
import qualified Type.Syntactic as S
import UnionFind

type Type s a = Var s (BoundNode s a)

type BoundNode s a = Node s (Bound s a)

data Bound s a b =
  Bound a {-# UNPACK #-} !(Var s (Binding b)) !(Term b) deriving Generic

instance Field1 (Bound s a b) (Bound s a' b) a a'
instance Field2 (Bound s a b) (Bound s a b) (Var s (Binding b)) (Var s (Binding b))
instance Field3 (Bound s a b) (Bound s a b) (Term b) (Term b)

binding :: Lens' (Bound s a b) (Var s (Binding b))
binding = _2

term :: Lens' (Bound s a b) (Term b)
term = _3

instance Foldable (Bound s a) where
  foldMap f = foldMap f . view term

data Binding a
  = Root
  | Binder !BindingFlag a deriving ( Show
                                   , Functor
                                   , Foldable
                                   , Traversable
                                   , Generic
                                   )

instance VariantA (Binding a) (Binding a) () ()
instance VariantB (Binding a) (Binding b) (BindingFlag, a) (BindingFlag, b)

root :: Prism' (Binding a) ()
root = _A

binder :: Prism (Binding a) (Binding b) (BindingFlag, a) (BindingFlag, b)
binder = _B

data Term a
  = Bot
  | Arr a a deriving ( Show
                     , Functor
                     , Foldable
                     , Traversable
                     , Generic
                     )

instance VariantA (Term a) (Term a) () ()
instance VariantB (Term a) (Term b) (a, a) (b, b)

bot :: Prism (Term a) (Term a) () ()
bot = _A

arr :: Prism (Term a) (Term b) (a, a) (b, b)
arr = _B

fromRestricted :: (MonadST m, MonadSupply Int m)
               => R.Type (Name a) -> m (Type (World m) (Maybe a))
fromRestricted =
  flip runReaderT mempty <<<
  fix (\ rec n b -> \ case
    R.Bot -> newBoundNodeSet n b Bot
    R.Var x -> asks (!x)
    R.Arr t_a t_b -> do
      p <- ask
      newBoundNodeSet n b $ Arr (p!t_a) (p!t_b)
    R.Forall x'@(Name _ n') bf o o' -> do
      n_a <- newBoundNodeSet Nothing Root Bot
      t' <- local (at x' ?~ n_a) $ rec n b o'
      same t' n_a >>= \ case
        True -> rec n' b o
        False -> do
          t <- rec n' (Binder bf t') o
          union t n_a
          return t') Nothing Root
  where
    newBoundNodeSet a b c = new =<< newBoundNode a b c
    newBoundNode a b c = Bound a <$> new b <*> pure c >>= newNode
    same var_a var_b = do
      ifM (var_a === var_b) (return True) $ do
        a <- var_a^!contents
        b <- var_b^!contents
        return $ a == b

toSyntactic :: MonadST m
            => Type (World m) (Maybe a)
            -> m (Product Int (S.PolyType (Product Int) (Name a)))
toSyntactic t0 = do
  bns <- t0^!boundNodes
  fix (\ rec n0 -> do
    t_s0 <- case n0^.projected.term of
      Bot -> return $ toInt n0 :* S.Bot
      Arr t_a t_b -> do
        n_a <- t_a^!contents
        n_b <- t_b^!contents
        return $ toInt n0 :* S.Mono (S.Arr (nodeVar n_a) (nodeVar n_b))
    foldrM (\ (bf, n) t_s -> nodeForall n bf <$> rec n <*> pure t_s)
      t_s0 (fromMaybe mempty $ bns^.at n0)) =<< t0^!contents
  where
    nodeVar n = toInt n :* S.Var (nodeName n)
    nodeForall n bf o o' = toInt n :* S.Forall (nodeName n) bf o o'
    nodeName n = Name (n^.int) (n^.projected._1)

syntactic :: MonadST m
          => IndexPreservingAction
             m
             (Type (World m) (Maybe a))
             (Product Int (S.PolyType (Product Int) (Name a)))
syntactic = act toSyntactic

boundNodes :: (MonadST m, s ~ World m)
           => IndexPreservingAction
              m
              (Type s a)
              (IntMap (BoundNode s a) [(BindingFlag, BoundNode s a)])
boundNodes = act $ perform (contents.preordered) >=> foldlM (\ ns ->
  perform contents >=> \ n -> n^!projected.binding.contents >>= \ case
    Root -> return ns
    Binder bf var' -> var'^!contents <&> \ n' -> ns&at n' %~ \ case
      Nothing -> Just [(bf, n)]
      Just bs -> Just ((bf, n) <| bs)) mempty
