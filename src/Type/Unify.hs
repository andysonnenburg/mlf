{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , RecordWildCards
  , TupleSections
  , TypeFamilies #-}
module Type.Unify
       ( UnifyError (..)
       , unify
       ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Comonad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Foldable (Foldable, foldl', foldlM, foldMap, for_, toList, traverse_)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>), mempty)

import Text.PrettyPrint.Free (Pretty (pretty), (<+>), text)

import Prelude hiding (read)

import Applicative ((<$$>))
import Hoist
import Id
import Int
import IntMap (IntMap, (!))
import qualified IntMap as Map
import IntSet (IntSet)
import qualified IntSet as Set
import Lens
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
  t_s <- extract' <$> toSyntactic t
  bs <- foldlM (\ bs -> find >=> read >=> \ n -> do
    b <- read <=< find $ n^.projected._2
    return $ Map.insert n b bs)
    mempty =<< preorder' t
  ancestors <- foldlM (\ ancestors -> find >=> read >=> \ n ->
    find (n^.projected._2) >>= read >>= \ case
      Root -> return $ Map.insert n mempty ancestors
      Binder _ s' -> do
        n' <- read =<< find s'
        return $ Map.insert n (Set.insert n' $ ancestors!n') ancestors)
    mempty =<< preorder' t
  ps <- getPermissions t
  S {..} <- execStateT (traversePairs_ unify' ts) emptyS
  rebind t bs merged =<< getPartiallyGrafted t grafted
  whenCyclic t $ cyclic t
  for_ graftedBots $ \ n -> unlessGreen (ps!n) $ t_s `doesNotSubsume` t
  preorder' t >>= traverse_ (find >=> read >=> \ n -> do
    b' <- read <=< find $ n^.projected._2
    whenRed (ps!n) $ unlessM (sameBinding (bs!n) b') $ t_s `doesNotSubsume` t)
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
    red = \ case
      R -> True
      _ -> False

unify' :: (MonadError (UnifyError a) m, MonadST m, s ~ World m)
       => Type s (Maybe a)
       -> Type s (Maybe a)
       -> Unify (Maybe a) m ()
unify' = fix $ \ rec x y -> do
  r_x <- find x
  r_y <- find y
  when (r_x /= r_y) $ do
    n_x <- read r_x
    let b_x = n_x^.projected
    n_y <- read r_y
    let b_y = n_y^.projected
    union (b_x^._2) (b_y^._2)
    case (b_x^._3, b_y^._3) of
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
  t_s' <- extract' <$> toSyntactic t
  throwError $ t_s `DoesNotSubsume` t_s'

rebind :: (MonadST m, s ~ World m)
       => Type s a
       -> IntMap (BoundNode s a) (Binding (Set s (BoundNode s a)))
       -> IntMap (BoundNode s a) (IntSet (BoundNode s a))
       -> IntMap (BoundNode s a) (IntSet (BoundNode s a))
       -> m ()
rebind t0 bs m b2 = void $ foldlM (\ s t -> do
  n <- read =<< find t
  b1_n <- fmap (flip lookupMany s . toList) $
          foldlM (\ ns' n_m -> case bs!n_m of
            Binder _ t' -> Set.insert <$> (read =<< find t') <*> pure ns'
            Root -> return ns')
          mempty $ fromMaybe mempty $ Map.lookup n m
  let b2_n = flip lookupMany s . maybe mempty toList $ Map.lookup n b2
      p = lca' (b1_n <> b2_n)
      bf = foldMap bindingFlag $ flip lookupMany bs $ toList $ m!n
      b' = case Path.uncons p of
        Just (_, n', _) -> Binder bf n'
        Nothing -> Root
  join $ write <$> find (n^.projected._2) <*> pure b'
  return $ Map.insert n (Path.cons (toInt n) t p) s) mempty =<< preorder' t0
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
    n <- read =<< find t
    whenM (gets $ Map.notMember n) $
      let c = n^.projected._3 in
      if Set.member n g
      then do
        modify $ insertEmpty n
        case c of
          Bot -> return ()
          Arr t_a t_b -> do
            t_a `graftedUnder` n
            t_b `graftedUnder` n
      else case c of
        Bot -> return ()
        Arr t_a t_b -> do
          rec t_a
          rec t_b) t0
  where
    t `graftedUnder` n' = do
      n <- read =<< find t
      walked <- gets $ Map.member n
      modify $ insertOne n n'
      unless walked $ case n^.projected._3 of
        Bot -> return ()
        Arr t_a t_b -> do
          t_a `graftedUnder` n
          t_b `graftedUnder` n
    insertOne k v = Map.alter (Just . \ case
      Nothing -> Set.singleton v
      Just vs -> Set.insert v vs) k
    insertEmpty k = Map.insert k mempty

type Unify a m = StateT (S (World m) a) m

data S s a = S { graftedBots :: IntSet (BoundNode s a)
               , grafted :: IntSet (BoundNode s a)
               , merged :: IntMap (BoundNode s a) (IntSet (BoundNode s a))
               }

emptyS :: S s a
emptyS = S mempty mempty mempty

graftedAt :: (MonadST m, s ~ World m)
          => BoundNode s a -> BoundNode s a -> Unify a m ()
n `graftedAt` bot = modify $ \ s@S {..} ->
  s { graftedBots = Set.insert bot graftedBots
    , grafted = Set.insert n grafted
    }

mergedInto :: (MonadST m, s ~ World m)
           => BoundNode s a -> BoundNode s a -> Unify a m ()
x `mergedInto` y = modify $ \ s@S {..} ->
  s { merged = Map.alter
               (Just .
                maybe (Set.insert x) (<>) (Map.lookup x merged) .
                fromMaybe (Set.singleton y)) y $
               Map.delete x merged }

whenCyclic :: MonadST m => Type (World m) a -> m () -> m ()
whenCyclic t0 m =
  flip runReaderT mempty $
  flip evalStateT mempty $
  fix (\ rec t -> do
    n <- read =<< find t
    whenM (gets $ Set.notMember n) $ do
      whenM (asks $ Set.member n) $ lift $ lift m
      case n^.projected._3 of
        Bot -> return ()
        Arr t_a t_b -> local (Set.insert n) $ rec t_a >> rec t_b
      modify $ Set.insert n) t0

preorder' :: (MonadST m, s ~ World m, Foldable f)
          => Set s (Node s f) -> m [Set s (Node s f)]
preorder' s = find s >>= read >>= \ n -> preorder n <$$> (s:)

traversePairs_ :: (Foldable t, Applicative f) => (a -> a -> f ()) -> t a -> f ()
traversePairs_ f = toList >>> \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs
