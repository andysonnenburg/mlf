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
import Control.Category ((>>>))
import Control.Comonad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Maybe (fromMaybe)
import Data.Semigroup (mempty)

import Text.PrettyPrint.Free

import Prelude hiding (read)

import Hoist
import Id
import Int
import qualified IntSet as Set
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

unify :: ( MonadError (UnifyError a) m
         , MonadST m
         , s ~ World m
         ) => Type s a -> [NodeSet s a] -> m (Type s a)
unify t@(t_n, t_b, t_bf) t_ns = do
  t_s <- extract' <$> toSyntactic t
  t_p <- getPaths t_n t_b
  p <- getPermissions t
  S {..} <- execStateT (traversePairs_ unify' t_ns) emptyS
  t_b' <- binders t_p merged <$> getPartiallyGrafted t_n grafted
  let t'@(_, _, t_bf') = (t_n, t_b', bindingFlags t_bf merged)
  whenCyclic t_n $ cyclic t'
  Set.for_ graftedBots $ \ x ->
    unlessGreen (p!x) $
      t_s `doesNotSubsume` t'
  forNode_ t_n $ toInt >>> \ x ->
    whenRed (p!x) $
      unless (Map.lookup x t_b == Map.lookup x t_b') $
        t_s `doesNotSubsume` t'
  forNode_ t_n $ toInt >>> \ x ->
    whenRed (p!x) $
      unless (Map.lookup x t_bf == Map.lookup x t_bf') $
        t_s `doesNotSubsume` t'
  return (t_n, t_b', t_bf')

unify' :: ( MonadError (UnifyError a) m
          , MonadST m
          , s ~ World m
          ) => NodeSet s a -> NodeSet s a -> Unify a m ()
unify' = fix $ \ rec x y -> do
  r_x <- find x
  r_y <- find y
  when (r_x /= r_y) $ do
    Node (toInt -> i_x) c_x <- read r_x
    Node (toInt -> i_y) c_y <- read r_y
    case (c_x, c_y) of
      (Bot, Bot) -> do
        i_y `mergedInto` i_x
        union x y
      (Bot, _) -> do
        i_x `mergedInto` i_y 
        i_y `graftedAt` i_x
        union y x
      (_, Bot) -> do
        i_y `mergedInto` i_x
        i_x `graftedAt` i_y
        union x y
      (Arr a_x b_x, Arr a_y b_y) -> do
        i_y `mergedInto` i_x
        union x y
        rec a_x a_y
        rec b_x b_y

cyclic :: ( MonadError (UnifyError a) m
          , MonadST m
          ) => Type (World m) a -> m b
cyclic = throwError . Cyclic . extract' <=< toSyntactic

doesNotSubsume :: ( MonadError (UnifyError a) m
                  , MonadST m
                  ) => PolyType Id (Name a) -> Type (World m) a -> m b
doesNotSubsume t_s t = do
  t_s' <- extract' <$> toSyntactic t
  throwError $ t_s `DoesNotSubsume` t_s'

bindingFlags :: BindingFlags -> IntMap IntSet -> BindingFlags
bindingFlags t_bf b1 =
  Map.mapWithKey
  (\ i bf0 -> maybe bf0 (Set.foldl' (\ bf -> (bf <>) . (t_bf!)) bf0) $ Map.lookup i b1)
  t_bf

getPartiallyGrafted :: MonadST m => NodeSet (World m) a -> IntSet -> m (IntMap IntSet)
getPartiallyGrafted t_n0 g =
  flip execStateT mempty $
  fix (\ rec t_n -> do
    Node (toInt -> x) c <- read =<< find t_n
    whenM (gets $ Map.notMember x) $
      if Set.member x g
      then do
        modify $ insertEmpty x
        case c of
          Bot -> return ()
          Arr a b -> do
            markPartiallyGrafted a x
            markPartiallyGrafted b x
      else case c of
        Bot -> return ()
        Arr a b -> do
          rec a
          rec b) t_n0
  where
    markPartiallyGrafted = fix $ \ rec t_n p -> do
      Node (toInt -> x) c <- read =<< find t_n
      marked <- gets $ Map.member x
      modify $ insertOne x p
      unless marked $
        case c of
          Bot -> return ()
          Arr a b -> do
            rec a x
            rec b x

insertOne :: Int -> Int -> IntMap IntSet -> IntMap IntSet
insertOne k v = Map.alter (Just . \ case
  Nothing -> Set.singleton v
  Just vs -> Set.insert v vs) k

insertEmpty :: Int -> IntMap IntSet -> IntMap IntSet
insertEmpty k = Map.insert k mempty

binders :: IntMap Path -> IntMap IntSet -> IntMap IntSet -> Binders
binders t_p b1 b2 = Map.mapMaybeWithKey (\ i p0 ->
  fmap fst $
  Path.uncons $
  Set.foldl' (\ p -> lca p . (t_p!)) p0 $
  fromMaybe mempty (Map.lookup i b1) <> fromMaybe mempty (Map.lookup i b2))
  t_p

getPaths :: MonadST m => NodeSet (World m) a -> IntMap Int -> m (IntMap Path)
getPaths t_n t_b = flip execStateT mempty $ forNode_' t_n $ toInt >>> \ x ->
  case Map.lookup x t_b of
    Nothing -> modify $ Map.insert x Path.empty
    Just y -> modify . Map.insert x =<< Path.cons y <$> gets (!y)

type Unify a m = StateT S m

data S = S { graftedBots :: IntSet
           , grafted :: IntSet
           , merged :: IntMap IntSet
           }

emptyS :: S
emptyS = S mempty mempty mempty

graftedAt :: Monad m => Int -> Int -> Unify a m ()
x `graftedAt` bot = modify $ \ s@S {..} ->
  s { graftedBots = Set.insert bot graftedBots
    , grafted = Set.insert x grafted
    }

mergedInto :: Monad m => Int -> Int -> Unify a m ()
x `mergedInto` y = modify $ \ s@S {..} ->
  s { merged = Map.alter
               (Just .
                maybe (Set.insert x) Set.union (Map.lookup x merged) .
                fromMaybe (Set.singleton y)) y $
               Map.delete x merged }

whenCyclic :: MonadST m => NodeSet (World m) a -> m () -> m ()
whenCyclic t_n0 m =
  flip runReaderT mempty $
  flip evalStateT mempty $
  fix (\ rec t_n -> do
    Node (toInt -> x) c <- read =<< find t_n
    whenM (gets $ Set.notMember x) $ do
      whenM (asks $ Set.member x) $ lift $ lift m
      case c of
        Bot -> return ()
        Arr a b ->
          local (Set.insert x) $ do
            rec a
            rec b
      modify $ Set.insert x) t_n0

traversePairs_ :: Applicative f => (a -> a -> f ()) -> [a] -> f ()
traversePairs_ f = \ case
  [] -> pure ()
  x0:xs -> fix (\ rec x -> \ case
    [] -> pure ()
    y:ys -> f x y *> rec y ys) x0 xs
