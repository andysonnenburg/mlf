{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Type.Permission
       ( Permission (..)
       , unlessGreen
       , whenRed
       , toScopedEffect
       , Permissions
       , getPermissions
       ) where

import Control.Applicative
import Control.Monad.State.Strict

import Data.Semigroup

import System.Console.Terminfo.Color (Color (ColorNumber))
import qualified System.Console.Terminfo.Color as Color
import System.Console.Terminfo.PrettyPrint

import Prelude hiding (read)

import Applicative
import IntMap (IntMap, (!))
import qualified IntMap as Map
import ST
import Type.Graphic
import qualified Type.Graphic.Postorder as Postorder
import qualified Type.Graphic.Preorder as Preorder
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
  M -> Color.White
  I -> Color.Yellow
  G -> Color.Green
  O -> orange
  R -> Color.Red

orange :: Color
orange = ColorNumber 202

type Permissions s a = IntMap (Node s a) Permission

data Down = Green | Orange | Red

fromDown :: Down -> Permission
fromDown = \ case
  Green -> G
  Orange -> O
  Red -> R

data Up = Monomorphic | Inert | Polymorphic

instance Semigroup Up where
  Polymorphic <> _ = Polymorphic
  _ <> Polymorphic = Polymorphic
  Inert <> _ = Inert
  _ <> Inert = Inert
  Monomorphic <> Monomorphic = Monomorphic

fromUp :: Permission -> Up -> Permission
fromUp x = \ case
  Monomorphic -> M
  Inert -> I
  Polymorphic -> x

getPermissions :: MonadST m => Type (World m) a -> m (Permissions (World m) a)
getPermissions t = do
  downs <- flip execStateT mempty $ Preorder.for_ t $ \ n@(Node _ b _) ->
    case b of
      Root -> modify $ Map.insert n Green
      Binder bf t' -> do
        n' <- read =<< find t'
        modify . Map.insert n =<< (bf,) <$> gets (!n') <$$> \ case
          (Flexible, Green) -> Green
          (Rigid, _) -> Orange
          (Flexible, _) -> Red
  ups <- flip execStateT mempty $ Postorder.for_ t $ \ n@(Node _ b c) -> do
    modify $ Map.alter (\ case
      _ | poly c -> Just Polymorphic
      Nothing -> Just Monomorphic
      Just up -> Just up) n
    case b of
      Root -> return ()
      Binder bf t' -> do
        n' <- read =<< find t'
        modify . Map.insertWith (<>) n' =<< (bf,) <$> gets (!n) <$$> \ case
          (_, Inert) -> Inert
          (_, Monomorphic) -> Monomorphic
          (Rigid, _) -> Inert
          _ -> Polymorphic
  return $ Map.intersectionWith (fromUp . fromDown) downs ups

poly :: Term s a -> Bool
poly = \ case
  Bot -> True
  _ -> False
