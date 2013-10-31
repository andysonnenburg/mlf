{-# LANGUAGE
    FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , Rank2Types
  , TypeFamilies #-}
module Type.Permission
       ( Permission (..)
       , unlessGreen
       , whenRed
       , toScopedEffect
       , Permissions
       , getPermissions
       ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Extras
import Control.Lens.Internal.Action
import Control.Monad.State.Strict

import Data.Foldable (foldlM, traverse_)
import Data.Profunctor.Unsafe ((#.))
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (First (..), mappend, mempty)

import qualified System.Console.Terminfo.Color as Terminfo
import System.Console.Terminfo.PrettyPrint

import Prelude hiding (read)

import Int
import IntMap (IntMap, (!))
import qualified IntMap as Map
import ST
import Type.Graphic
import UnionFind

data Permission = M | I | G | O | R deriving Show

unlessGreen :: Monad m => Permission -> m () -> m ()
unlessGreen G _ = return ()
unlessGreen _ m = m

whenRed :: Monad m => Permission -> m () -> m ()
whenRed R m = m
whenRed _ _ = return ()

toScopedEffect :: Permission -> ScopedEffect
toScopedEffect = soft . Foreground . \ case
  M -> Terminfo.White
  I -> Terminfo.Yellow
  G -> Terminfo.Green
  O -> orange
  R -> Terminfo.Red

orange :: Terminfo.Color
orange = Terminfo.ColorNumber 202

type Permissions s f = IntMap (Node s f) Permission

data Color = Green | Orange | Red

fromColor :: Color -> Permission
fromColor = \ case
  Green -> G
  Orange -> O
  Red -> R

data Morphism = Monomorphic | Inert | Polymorphic

instance Semigroup Morphism where
  Polymorphic <> _ = Polymorphic
  _ <> Polymorphic = Polymorphic
  Inert <> _ = Inert
  _ <> Inert = Inert
  Monomorphic <> Monomorphic = Monomorphic

fromMorphism :: Permission -> Morphism -> Permission
fromMorphism x = \ case
  Monomorphic -> M
  Inert -> I
  Polymorphic -> x

getPermissions :: (MonadST m, s ~ World m)
               => Type s a -> m (Permissions s (Bound s a))
getPermissions t = do
  n0 <- t^!ref.contents
  colors <- foldlM (\ colors -> perform (ref.contents) >=> \ n ->
    n^!projected.binding.ref.contents >>= \ case
      Root -> return $ colors&at n ?~ Green
      Binder bf s' -> do
        n' <- s'^!ref.contents
        return $ colors&at n ?~ case (bf, colors!n') of
          (Flexible, Green) -> Green
          (Rigid, _) -> Orange
          (Flexible, _) -> Red) mempty =<< n0^!preordered.to (t <|)
  morphisms <- flip execStateT mempty $ traverse_ (perform (ref.contents) >=> \ n -> do
    at n %= \ case
      _ | is bot $ n^.projected.term -> Just Polymorphic
      Nothing -> Just Monomorphic
      Just morphism -> Just morphism
    whenM (projected.binding.ref.contents.binder) n $ \ (bf, s') -> do
      morphisms <- get
      n' <- s'^!ref.contents
      at' n' ?= case (bf, morphisms!n) of
        (_, Inert) -> Inert
        (_, Monomorphic) -> Monomorphic
        (Rigid, _) -> Inert
        _ -> Polymorphic) =<< n0^!postordered.to (|> t)
  return $ Map.intersectionWith (fromMorphism . fromColor) colors morphisms

at' :: (IsInt k, Semigroup v) => k -> Lens' (IntMap k v) (Maybe v)
{-# INLINE at' #-}
at' k f m = f mv <&> \ r -> case r of
  Nothing -> maybe m (const (Map.delete k m)) mv
  Just v' -> Map.insertWith (<>) k v' m
  where
    mv = Map.lookup k m

whenM :: Monad m => FirstActing m a s a -> s -> (a -> m ()) -> m ()
{-# INLINE whenM #-}
whenM l s f = (getFirstEffect #. l (FirstEffect #. return . First #. Just)) s >>= (\ case
  Nothing -> return ()
  Just a -> f a) . getFirst

type FirstActing m r s a = LensLike (FirstEffect m r) s s a a

newtype FirstEffect m r a = FirstEffect { getFirstEffect :: m (First r) }

instance Functor (FirstEffect m r) where
  {-# INLINE fmap #-}
  fmap _ = FirstEffect . getFirstEffect

instance Contravariant (FirstEffect m r) where
  {-# INLINE contramap #-}
  contramap _ = FirstEffect . getFirstEffect

instance Monad m => Effective m (First r) (FirstEffect m r) where
  {-# INLINE effective #-}
  effective = FirstEffect
  {-# INLINE ineffective #-}
  ineffective = getFirstEffect

instance Monad m => Applicative (FirstEffect m r) where
  {-# INLINE pure #-}
  pure _ = FirstEffect (return mempty)
  {-# INLINE (<*>) #-}
  f <*> a = FirstEffect $ liftM2 mappend (getFirstEffect f) (getFirstEffect a)
