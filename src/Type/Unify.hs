{-# LANGUAGE
    DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , Rank2Types
  , TypeFamilies #-}
module Type.Unify
       ( UnifyError (..)
       , unify
       ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Lens
import Control.Lens.Extras
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Semigroup (Monoid, Semigroup, (<>), mempty)

import GHC.Generics (Generic)

import Text.PrettyPrint.Free (Pretty (pretty), (<+>), text)

import Prelude hiding (foldr, read)

import Hoist
import Id
import Int
import IntMap (IntMap, (!))
import qualified IntMap as Map
import IntSet (IntSet)
import qualified IntSet as Set
import List (list)
import Monad
import Name
import Path (Path, lca)
import qualified Path
import ST
import Type.Graphic
import Type.Permission
import Type.Syntactic (MonoType, PolyType)
import UnionFind

data UnifyError a
  = Cyclic (PolyType Id (Name a))
  | MonoType Id (Name a) `IsNot` MonoType Id (Name a)
  | PolyType Id (Name a) `DoesNotSubsume` PolyType Id (Name a)
  deriving Show

instance Pretty a => Pretty (UnifyError a) where
  pretty = \ case
    Cyclic x ->
      pretty x <+> text "has" <+> text "a" <+> text "cycle"
    x `IsNot` y ->
      pretty x <+> text "is" <+> text "not" <+> pretty y
    x `DoesNotSubsume` y ->
      pretty x <+> text "does" <+> text "not" <+> text "subsume" <+> pretty y

unify :: (MonadError (UnifyError a) m, MonadST m, s ~ World m)
      => Type s (Maybe a) -> [Type s (Maybe a)] -> m ()
unify t ts = do
  t_s <- t^!syntactic.extracted'

  ps <- t^!permissions

  bs <- mexecStateT $ t^!preordered'.folded.contents.act (\ n ->
    (at n ?=) =<< n^!projected.binding.contents)

  bs_plus <- mexecStateT $ t^!preordered'.folded.contents.act (\ n ->
    n^!projected.binding.contents >>= \ case
      Root -> at n ?= mempty
      Binder _ t' -> do
        n' <- t'^!contents
        gets (!n') <&> contains n' .~ True >>= (at n ?=))

  s <- execStateT (traversePairs_ unifyTypes ts) emptyS

  rebind t bs (s^.merged) (s^.grafted)

  let whenCyclic m = flip runReaderT mempty $ mevalStateT $
        fix (\ rec -> perform contents >=> \ n ->
          whenM (gets $ Set.notMember n) $ do
            whenM (asks $ Set.member n) $ lift $ lift m
            local (contains n .~ True) (forOf_ (projected.term.folded) n rec)
            contains n .= True) t

      whenIllegalGraft m = forOf_ (graftedBots.folded) s $ \ n ->
        unless (ps!n&is green) m

      whenIllegalRaise m = t^!preordered'.folded.contents.act (\ n ->
        forOf_ (merged.ix n.folded) s $ \ m_n -> when (ps!m_n&is red) $ do
          b <- m_n^!projected.binding.contents
          unlessM (sameBinder (bs!m_n) b) m)

      whenIllegalWeaken m = t^!preordered'.folded.contents.act (\ n ->
        forOf_ (merged.ix n.folded) s $ \ n_m -> when (ps!n_m&is red) $ do
          b <- n_m^!projected.binding.contents
          unlessM (sameBindingFlag (bs!n_m) b) m)

      whenIllegalMerge m = t^!preordered'.folded.contents.act (\ n ->
        n^!projected.binding.contents.binder._2.contents.act (\ n' ->
          let m_ns = s^.merged.ix n
              m_ns' = fromMaybe (Set.singleton n') $ s^?merged.ix n' in
          flip evalStateT Map.empty $ for_ m_ns $ \ m_n ->
            for_ (Set.intersection (bs_plus!m_n) m_ns') $ \ m_n' ->
              use (at m_n') >>= \ case
                Just True -> m
                _ -> at m_n' ?= (ps!m_n&is red)))

  whenCyclic $ cyclic t
  whenIllegalGraft $ t_s `doesNotSubsume` t
  whenIllegalRaise $ t_s `doesNotSubsume` t
  whenIllegalWeaken $ t_s `doesNotSubsume` t
  whenIllegalMerge $ t_s `doesNotSubsume` t
  where
    sameBinder = curry $ \ case
      (Root, Root) -> return True
      (Binder _ t_x, Binder _ t_y) -> same t_x t_y
      _ -> return False
    sameBindingFlag = curry $ \ case
      (Root, Root) -> return True
      (Binder bf_x _, Binder bf_y _) -> return $ bf_x == bf_y
      _ -> return False
    same var_x var_y = ifM (var_x === var_y) (return True) $
      (==) <$> var_x^!contents <*> var_y^!contents

unifyTypes :: (MonadError (UnifyError a) m, MonadST m, s ~ World m)
           => Type s (Maybe a)
           -> Type s (Maybe a)
           -> Unify (Maybe a) m ()
unifyTypes = fix $ \ rec var_x var_y -> do
  unlessM (var_x === var_y) $ do
    n_x <- var_x^!contents
    let b_x = n_x^.projected
    n_y <- var_y^!contents
    let b_y = n_y^.projected
    union (b_x^.binding) (b_y^.binding)
    case (b_x^.term, b_y^.term) of
      (Bot, Bot) -> do
        n_y `mergedInto` n_x
        union var_x var_y
      (Bot, _) -> do
        n_x `mergedInto` n_y
        n_y `graftedAt` n_x
        union var_y var_x
      (_, Bot) -> do
        n_y `mergedInto` n_x
        n_x `graftedAt` n_y
        union var_x var_y
      (Arr t_x t_x', Arr t_y t_y') -> do
        n_y `mergedInto` n_x
        union var_x var_y
        rec t_x t_y
        rec t_x' t_y'

rebind :: (MonadST m, s ~ World m)
       => Type s a
       -> IntMap (BoundNode s a) (Binding (Var s (BoundNode s a)))
       -> IntMap (BoundNode s a) (IntSet (BoundNode s a))
       -> IntSet (BoundNode s a)
       -> m ()
rebind t0 bs m g = do
  b2 <- getPartiallyGrafted t0 g
  mevalStateT $ t0^!preordered'.folded.act (\ t -> do
    n <- t^!contents
    let m_n = fromMaybe (Set.singleton n) $ m^.at n
    b1_n <- m_n^!!folded.projected.binding.contents.binder._2.contents.getElem
    b2_n <- b2^!!ix n.folded.getElem
    let path = lca' $ b1_n <> b2_n
        bf = m_n^.folded.to (bs!).binder._1
        b = maybe Root (Binder bf) $ path^.to Path.uncons&mapped %~ view _2
    join $ write <$> n^!projected.binding <*> pure b
    at n ?= Path.cons (n^.int) t path)
  where
    getElem = act $ gets . flip (!)

getPartiallyGrafted :: (MonadST m, s ~ World m)
                    => Type (World m) a
                    -> IntSet (BoundNode s a)
                    -> m (IntMap (BoundNode s a) (IntSet (BoundNode s a)))
getPartiallyGrafted t0 g = mexecStateT $ fix (\ rec t -> do
  n <- t^!contents
  whenM (gets $ Map.notMember n) $ do
    at n .= mempty
    forOf_ (projected.term.folded) n $
      if g^.contains n then (`graftedUnder` n) else rec) t0
  where
    t `graftedUnder` n' = do
      n <- t^!contents
      walked <- use $ contains n
      at n %= Just . \ case
        Nothing -> Set.singleton n'
        Just ns' -> ns'&contains n' .~ True
      unless walked $ forOf_ (projected.term.folded) n (`graftedUnder` n)

cyclic :: (MonadError (UnifyError a) m, MonadST m)
       => Type (World m) (Maybe a) -> m b
cyclic = throwError . Cyclic . extract' <=< toSyntactic

doesNotSubsume :: (MonadError (UnifyError a) m, MonadST m)
               => PolyType Id (Name a)
               -> Type (World m) (Maybe a)
               -> m b
doesNotSubsume t_s t = do
  t_s' <- t^!syntactic.extracted'
  throwError $ t_s `DoesNotSubsume` t_s'

type Unify a m = StateT (S (World m) a) m

data S s a =
  S
  (IntSet (BoundNode s a))
  (IntSet (BoundNode s a))
  (IntMap (BoundNode s a) (IntSet (BoundNode s a))) deriving Generic

instance Field1 (S s a) (S s a) (IntSet (BoundNode s a)) (IntSet (BoundNode s a))
instance Field2 (S s a) (S s a) (IntSet (BoundNode s a)) (IntSet (BoundNode s a))
instance Field3
         (S s a)
         (S s a)
         (IntMap (BoundNode s a) (IntSet (BoundNode s a)))
         (IntMap (BoundNode s a) (IntSet (BoundNode s a)))

graftedBots :: Lens' (S s a) (IntSet (BoundNode s a))
graftedBots = _1

grafted :: Lens' (S s a) (IntSet (BoundNode s a))
grafted = _2

merged :: Lens' (S s a) (IntMap (BoundNode s a) (IntSet (BoundNode s a)))
merged = _3

emptyS :: S s a
emptyS = S mempty mempty mempty

graftedAt :: (MonadST m, s ~ World m)
          => BoundNode s a -> BoundNode s a -> Unify a m ()
n `graftedAt` x = modify $ \ s -> s
  &graftedBots.contains x .~ True
  &grafted.contains n .~ True

mergedInto :: (MonadST m, s ~ World m)
           => BoundNode s a -> BoundNode s a -> Unify a m ()
x `mergedInto` y = modify $ \ s -> s
  &merged.at x .~ Nothing
  &merged.at y %~ Just .
                  maybe (contains x .~ True) (<>) (s^.merged.at x) .
                  fromMaybe (Set.singleton y)

preordered' :: (MonadST m, s ~ World m, Foldable f)
            => IndexPreservingAction m (Var s (Node s f)) [Var s (Node s f)]
preordered' = act $ \ t -> t^!contents.preordered.to (t <|)

traversePairs_ :: (Foldable t, Applicative f) => (a -> a -> f ()) -> t a -> f ()
traversePairs_ f = toList >>> \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs

mevalStateT :: (Monoid s, Monad m) => StateT s m a -> m a
mevalStateT = flip evalStateT mempty

mexecStateT :: (Monoid s, Monad m) => StateT s m a -> m s
mexecStateT = flip execStateT mempty

lca' :: Foldable f => f (Path a) -> Path a
lca' = list Path.empty (foldl' lca) . toList
