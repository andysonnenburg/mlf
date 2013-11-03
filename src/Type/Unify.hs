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
import Control.Comonad
import Control.Lens
import Control.Lens.Extras
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Foldable hiding (find)
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
import Path (lca)
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

  bs <- foldlM (\ bs -> perform (ref.contents) >=> \ n -> do
    b <- n^!projected.binding.ref.contents
    return $ bs&at n ?~ b) mempty =<< t^!preordered'

  bs_plus <- foldlM (\ bs_plus -> perform (ref.contents) >=> \ n ->
    n^!projected.binding.ref.contents >>= \ case
      Root -> return $ bs_plus&at n ?~ mempty
      Binder _ s' -> do
        n' <- s'^!ref.contents
        return $ bs_plus&at n ?~ (bs_plus!n'&contains n' .~ True))
    mempty =<< t^!preordered'

  s <- execStateT (traversePairs_ unify' ts) emptyS

  rebind t bs (s^.merged) =<< getPartiallyGrafted t (s^.grafted)

  whenCyclic t $ cyclic t

  forOf_ (graftedBots.folded) s $ \ n -> unless (ps!n&is green) $
    t_s `doesNotSubsume` t

  traverse_ (perform (ref.contents) >=> \ n ->
    forOf_ (merged.ix n.folded) s $ \ m -> when (ps!m&is red) $ do
      b' <- m^!projected.binding.ref.contents
      unlessM (sameBinding (bs!m) b') $
        t_s `doesNotSubsume` t) =<< t^!preordered'

  traverse_ (perform (ref.contents) >=> \ n ->
    n^!?projected.binding.ref.contents.binder._2 >>=
    traverse_ (perform (ref.contents) >=> \ n' -> do
      let ms = s^.merged.ix' n
          ms' = fromMaybe (Set.singleton n') $ s^?merged.ix n'

          insert m' m t_up = case Map.lookup m' t_up of
            Just True -> t_s `doesNotSubsume` t
            _ -> return $ t_up&at m' ?~ (ps!m&is red)

      void $ foldlM (\ t_up m -> foldlM (flip $ \ m' -> insert m' m) t_up $
        Set.intersection (bs_plus!m) ms')
        mempty ms)) =<< t^!preordered'
  where
    sameBinding = curry $ \ case
      (Root, Root) -> return True
      (Binder bf t_b, Binder bf' t_b')
        | bf == bf' -> same t_b t_b'
      _ -> return False
    same t_a t_b = do
      r_a <- find t_a
      r_b <- find t_b
      if r_a == r_b
        then return True
        else (==) <$> read r_a <*> read r_b

unify' :: (MonadError (UnifyError a) m, MonadST m, s ~ World m)
       => Type s (Maybe a)
       -> Type s (Maybe a)
       -> Unify (Maybe a) m ()
unify' = fix $ \ rec x y -> do
  r_x <- x^!ref
  r_y <- y^!ref
  when (r_x /= r_y) $ do
    n_x <- r_x^!contents
    let b_x = n_x^.projected
    n_y <- r_y^!contents
    let b_y = n_y^.projected
    union (b_x^.binding) (b_y^.binding)
    case (b_x^.term, b_y^.term) of
      (Bot, Bot) -> do
        n_y `mergedInto` n_x
        union x y
      (Bot, _) -> do
        n_x `mergedInto` n_y
        n_y `graftedAt` n_x
        union y x
      (_, Bot) -> do
        n_y `mergedInto` n_x
        n_x `graftedAt` n_y
        union x y
      (Arr t_x t_x', Arr t_y t_y') -> do
        n_y `mergedInto` n_x
        union x y
        rec t_x t_y
        rec t_x' t_y'

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

rebind :: (MonadST m, s ~ World m)
       => Type s a
       -> IntMap (BoundNode s a) (Binding (Set s (BoundNode s a)))
       -> IntMap (BoundNode s a) (IntSet (BoundNode s a))
       -> IntMap (BoundNode s a) (IntSet (BoundNode s a))
       -> m ()
rebind t0 bs m b2 = void $ foldlM (\ ps t -> do
  n <- t^!ref.contents

  b1_n <- fmap (flip lookupMany ps) $
          foldlM (\ ns' n_m -> case bs!n_m of
            Binder _ t' -> Set.insert <$> (t'^!ref.contents) <*> pure ns'
            Root -> return ns')
          mempty $ Map.findWithDefault (Set.singleton n) n m

  let b2_n = flip lookupMany ps $ b2^.ix' n

      p = lca' $ b1_n <> b2_n

      bf = foldMap bindingFlag $
           flip lookupMany bs $
           Map.findWithDefault (Set.singleton n) n m
      b' = case Path.uncons p of
        Just (_, n', _) -> Binder bf n'
        Nothing -> Root

  join $ write <$> n^!projected.binding.ref <*> pure b'

  return $ ps&at n ?~ Path.cons (n^.int) t p) mempty =<< t0^!preordered'
  where
    lca' = list Path.empty (foldl' lca)
    bindingFlag = \ case
      Root -> Flexible
      Binder bf _ -> bf
    lookupMany ks xs = foldr (\ k b -> maybe b (:b) $ Map.lookup k xs) mempty ks

getPartiallyGrafted :: (MonadST m, s ~ World m)
                    => Type (World m) a
                    -> IntSet (BoundNode s a)
                    -> m (IntMap (BoundNode s a) (IntSet (BoundNode s a)))
getPartiallyGrafted t0 g =
  flip execStateT mempty $
  fix (\ rec t -> do
    n <- t^!ref.contents
    whenM (gets $ Map.notMember n) $
      let c = n^.projected.term in
      if g^.contains n
      then do
        at n .= mempty
        traverse_ (`graftedUnder` n) c
      else traverse_ rec c) t0
  where
    t `graftedUnder` n' = do
      n <- t^!ref.contents
      walked <- use $ contains n
      at n %= Just . \ case
        Nothing -> Set.singleton n'
        Just ns' -> ns'&contains n' .~ True
      unless walked $ traverse_ (`graftedUnder` n) $ n^.projected.term

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

whenCyclic :: MonadST m => Type (World m) a -> m () -> m ()
whenCyclic t0 m =
  flip runReaderT mempty $
  flip evalStateT mempty $
  fix (\ rec t -> do
    n <- t^!ref.contents
    whenM (gets $ Set.notMember n) $ do
      whenM (asks $ Set.member n) $ lift $ lift m
      local (contains n .~ True) (forOf_ (projected.term.folded) n rec)
      contains n .= True) t0

preordered' :: (MonadST m, s ~ World m, Foldable f)
            => IndexPreservingAction m (Set s (Node s f)) [Set s (Node s f)]
preordered' = act $ \ t -> t^!ref.contents.preordered.to (t <|)

traversePairs_ :: (Foldable t, Applicative f) => (a -> a -> f ()) -> t a -> f ()
traversePairs_ f = toList >>> \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs

ix' :: (IsInt k, Monoid v) => k -> Lens' (IntMap k v) v
{-# INLINE ix' #-}
ix' k f m = f (fromMaybe mempty (Map.lookup k m)) <&> \ v' -> Map.insert k v' m
