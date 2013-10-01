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
import Control.Comonad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Foldable (fold, foldl', foldlM, for_, toList)
import Data.Maybe (fromMaybe)
import Data.Semigroup (mempty)

import Text.PrettyPrint.Free

import Prelude hiding (read)

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
import qualified Type.Graphic.Preorder as Preorder
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

unify :: (MonadError (UnifyError a) m, MonadFix m, MonadST m, s ~ World m)
      => Type s a -> [NodeSet s a] -> m ()
unify t ts = do
  t_s <- extract' <$> toSyntactic t
  bs <- Preorder.foldl' (\ bs n@(Node _ b _) -> Map.insert n b bs) mempty t
  p <- getPermissions t
  (S {..}, _) <- mfix $ \ ~(_, bs') -> do
    s@S {..} <- execStateT (traversePairs_ (unify' bs') ts) emptyS
    fmap (s,) . getBindings t bs merged =<< getPartiallyGrafted t grafted
  whenCyclic t $ cyclic t
  for_ graftedBots $ \ n ->
    unlessGreen (p!n) $
      t_s `doesNotSubsume` t
  forNode_ t $ \ n@(Node _ b' _) ->
    whenRed (p!n) $ do
      case (bs!n, b') of
        (Root, Root) -> return ()
        (Binder bf t_p, Binder bf' t_p')
          | bf == bf' -> do
            n_p <- read =<< find t_p
            n_p' <- read =<< find t_p'
            when (n_p /= n_p') $ t_s `doesNotSubsume` t
        _ -> t_s `doesNotSubsume` t

unify' :: (MonadError (UnifyError a) m, MonadST m, s ~ World m)
       => IntMap (Node s a) (Binding s a)
       -> NodeSet s a
       -> NodeSet s a
       -> Unify a m ()
unify' bs = fix $ \ rec x y -> do
  r_x <- find x
  r_y <- find y
  when (r_x /= r_y) $ do
    n_x@(Node _ _ c_x) <- read r_x
    n_y@(Node _ _ c_y) <- read r_y
    case (c_x, c_y) of
      (Bot, Bot) -> do
        n_y `mergedInto` n_x
        union x y
        writeBinding x $ bs!n_x
      (Bot, _) -> do
        n_x `mergedInto` n_y
        n_y `graftedAt` n_x
        union y x
        writeBinding y $ bs!n_y
      (_, Bot) -> do
        n_y `mergedInto` n_x
        n_x `graftedAt` n_y
        union x y
        writeBinding x $ bs!n_x
      (Arr t_x t_x', Arr t_y t_y') -> do
        n_y `mergedInto` n_x
        union x y
        writeBinding x $ bs!n_x
        rec t_x t_y
        rec t_x' t_y'

cyclic :: (MonadError (UnifyError a) m, MonadST m)
       => Type (World m) a -> m b
cyclic = throwError . Cyclic . extract' <=< toSyntactic

doesNotSubsume :: (MonadError (UnifyError a) m, MonadST m)
               => PolyType Id (Name a)
               -> Type (World m) a
               -> m b
doesNotSubsume t_s t = do
  t_s' <- extract' <$> toSyntactic t
  throwError $ t_s `DoesNotSubsume` t_s'

getBindings :: (MonadST m, s ~ World m)
            => Type (World m) a
            -> IntMap (Node s a) (Binding s a)
            -> IntMap (Node s a) (IntSet (Node s a))
            -> IntMap (Node s a) (IntSet (Node s a))
            -> m (IntMap (Node s a) (Binding s a))
getBindings t0 bs m b2 =
  fmap fst <$>
  Preorder.foldlSM (\ s t -> do
    n <- read =<< find t
    b1_n <- fmap (fmap snd . flip lookupList s . toList) $
            foldlM (foldlM (\ ns' n_m -> case bs!n_m of
                               Binder _ t' -> do
                                 n' <- read =<< find t'
                                 return $ Set.insert n' ns'
                               Root -> return ns'))
            mempty
            m
    let b2_n = fmap snd . flip lookupList s . maybe mempty toList $ Map.lookup n b2
        p = case b1_n <> b2_n of
          [] -> Path.empty
          x:xs -> foldl' lca x xs
        b = case (fold $ flip lookupList bs $ toList $ m!n, Path.uncons p) of
          (Binder bf _, Just (_, n', _)) -> Binder bf n'
          (Root, _) -> Root
          (_, Nothing) -> Root
    return $ Map.insert n (b, Path.cons (toInt n) t p) s) mempty t0
  where
    lookupList ks xs = foldr (\ k b -> maybe b (:b) $ Map.lookup k xs) [] ks

getPartiallyGrafted :: (MonadST m, s ~ World m)
                    => Type (World m) a
                    -> IntSet (Node s a)
                    -> m (IntMap (Node s a) (IntSet (Node s a)))
getPartiallyGrafted t0 g =
  flip execStateT mempty $
  fix (\ rec t -> do
    n@(Node _ _ c) <- read =<< find t
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
    t `graftedUnder` p = do
      n@(Node _ _ c) <- read =<< find t
      walked <- gets $ Map.member n
      modify $ insertOne n p
      unless walked $
        case c of
          Bot -> return ()
          Arr t_a t_b -> do
            t_a `graftedUnder` n
            t_b `graftedUnder` n
    insertOne k v = Map.alter (Just . \ case
      Nothing -> Set.singleton v
      Just vs -> Set.insert v vs) k
    insertEmpty k = Map.insert k mempty

type Unify a m = StateT (S (World m) a) m

data S s a = S { graftedBots :: IntSet (Node s a)
               , grafted :: IntSet (Node s a)
               , merged :: IntMap (Node s a) (IntSet (Node s a))
               }

emptyS :: S s a
emptyS = S mempty mempty mempty

graftedAt :: (MonadST m, s ~ World m)
          => Node s a -> Node s a -> Unify a m ()
n `graftedAt` bot = modify $ \ s@S {..} ->
  s { graftedBots = Set.insert bot graftedBots
    , grafted = Set.insert n grafted
    }

mergedInto :: (MonadST m, s ~ World m)
           => Node s a -> Node s a -> Unify a m ()
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
  fix (\ rec t_n -> do
    n@(Node _ _ c) <- read =<< find t_n
    whenM (gets $ Set.notMember n) $ do
      whenM (asks $ Set.member n) $ lift $ lift m
      case c of
        Bot -> return ()
        Arr a b ->
          local (Set.insert n) $ do
            rec a
            rec b
      modify $ Set.insert n) t0

writeBinding :: (MonadST m, s ~ World m)
             => Set s (Node s a) -> Binding s a -> m ()
writeBinding set b = do
  Node a _ c <- read =<< find set
  write set $ Node a b c

traversePairs_ :: Applicative f => (a -> a -> f ()) -> [a] -> f ()
traversePairs_ f = \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs
