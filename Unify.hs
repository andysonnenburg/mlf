{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , RecordWildCards
  , TupleSections
  , TypeFamilies
  , ViewPatterns #-}
module Unify
       ( UnifyError (..)
       , unify
       ) where

import Control.Applicative
import Control.Category ((<<<), (>>>))
import Control.Comonad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Foldable (Foldable, fold, foldl', foldlM, for_, toList, traverse_)
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
          b <- read <=< find <<< boundBinding $ project n
          return $ Map.insert n b bs) mempty =<< preorder' t
  p <- getPermissions t
  S {..} <- execStateT (traversePairs_ unify' ts) emptyS
  rebind t bs merged =<< getPartiallyGrafted t grafted
  whenCyclic t $ cyclic t
  for_ graftedBots $ \ n -> unlessGreen (p!n) $ t_s `doesNotSubsume` t
  preorder' t >>= traverse_ (find >=> read >=> \ n -> do
    b' <- read <=< find <<< boundBinding $ project n
    whenRed (p!n) $ case (bs!n, b') of
      (Root, Root) -> return ()
      (Binder bf t_b, Binder bf' t_b')
        | bf == bf' -> whenM (notSame t_b t_b') $ t_s `doesNotSubsume` t
      _ -> t_s `doesNotSubsume` t)
  where
    notSame t_a t_b = do
      r_a <- find t_a
      r_b <- find t_b
      if r_a /= r_b
        then return True
        else (/=) <$> read r_a <*> read r_b

unify' :: (MonadError (UnifyError a) m, MonadST m, s ~ World m)
       => Type s (Maybe a)
       -> Type s (Maybe a)
       -> Unify (Maybe a) m ()
unify' = fix $ \ rec x y -> do
  r_x <- find x
  r_y <- find y
  when (r_x /= r_y) $ do
    n_x <- read r_x
    let b_x = project n_x
    n_y <- read r_y
    let b_y = project n_y
    union (boundBinding b_x) (boundBinding b_y)
    case (boundTerm b_x, boundTerm b_y) of
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

type BoundNode s a = Node s (Bound s a)

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
      b' = case (fold $ flip lookupMany bs $ toList $ m!n, Path.uncons p) of
        (Binder bf _, Just (_, n', _)) -> Binder bf n'
        (Root, _) -> Root
        (_, Nothing) -> Root
  join $ write <$> find (boundBinding $ project n) <*> pure b'
  return $ Map.insert n (Path.cons (toInt n) t p) s) mempty =<< preorder' t0
  where
    lca' = list Path.empty (foldl' lca)
    lookupMany ks xs = foldr (\ k b -> maybe b (:b) $ Map.lookup k xs) [] ks
    list nil cons = \ case
      [] -> nil
      x:xs -> cons x xs

getPartiallyGrafted :: (MonadST m, s ~ World m)
                    => Type (World m) a
                    -> IntSet (BoundNode s a)
                    -> m (IntMap (BoundNode s a) (IntSet (BoundNode s a)))
getPartiallyGrafted t0 g =
  flip execStateT mempty $
  fix (\ rec t -> do
    n <- read =<< find t
    let c = boundTerm $ project n
    whenM (gets $ Map.notMember n) $
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
      unless walked $
        case boundTerm $ project n of
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
      case boundTerm $ project n of
        Bot -> return ()
        Arr t_a t_b -> local (Set.insert n) $ rec t_a >> rec t_b
      modify $ Set.insert n) t0

boundBinding :: Bound s a b -> Set s (Binding b)
boundBinding (Bound _ x _) = x

boundTerm :: Bound s a b -> Term b
boundTerm (Bound _ _ x) = x

preorder' :: (MonadST m, s ~ World m, Foldable f)
          => Set s (Node s f) -> m [Set s (Node s f)]
preorder' s = find s >>= read >>= \ n -> preorder n <$$> (s:)

traversePairs_ :: (Foldable t, Applicative f) => (a -> a -> f ()) -> t a -> f ()
traversePairs_ f = toList >>> \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs
