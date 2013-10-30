{-# LANGUAGE LambdaCase, TupleSections, TypeFamilies #-}
module Type.Permission
       ( Permission (..)
       , unlessGreen
       , whenRed
       , toScopedEffect
       , Permissions
       , getPermissions
       ) where

import Control.Applicative
import Control.Category ((<<<), (>>>))
import Control.Lens
import Control.Monad.State.Strict

import Data.Foldable (foldlM, traverse_)
import Data.Semigroup

import qualified System.Console.Terminfo.Color as Terminfo
import System.Console.Terminfo.PrettyPrint

import Prelude hiding (read)

import Applicative
import Function (($$))
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
  n0 <- read =<< find t
  colors <- foldlM (\ colors -> find >=> read >=> \ n ->
    n^.projected._2 $$ find >=> read >=> \ case
      Root -> return $ Map.insert n Green colors
      Binder bf s' -> find s' >>= read $$ fmap $
        flip (Map.insert n) colors <<< (colors!) >>> (bf,) >>> \ case
          (Flexible, Green) -> Green
          (Rigid, _) -> Orange
          (Flexible, _) -> Red) mempty =<< (t:) <$> preorder n0
  morphisms <- flip execStateT mempty $ traverse_ (find >=> read >=> \ n -> do
    modify $ Map.alter (\ case
      _ | poly $ n^.projected._3 -> Just Polymorphic
      Nothing -> Just Monomorphic
      Just morphism -> Just morphism) n
    n^.projected._2 $$ find >=> read >=> \ case
      Root -> return ()
      Binder bf s' -> do
        n' <- read =<< find s'
        modify . Map.insertWith (<>) n' =<< (bf,) <$> gets (!n) <$$> \ case
          (_, Inert) -> Inert
          (_, Monomorphic) -> Monomorphic
          (Rigid, _) -> Inert
          _ -> Polymorphic) =<< (++ [t]) <$> postorder n0
  return $ Map.intersectionWith (fromMorphism . fromColor) colors morphisms

poly :: Term a -> Bool
poly = \ case
  Bot -> True
  _ -> False
