{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Type.Permission
       ( Permission (..)
       , toScopedEffect
       , Permissions
       , getPermissions
       ) where

import Control.Applicative
import Control.Monad.State.Strict

import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import Data.Semigroup

import qualified System.Console.Terminfo.Color as Color
import System.Console.Terminfo.PrettyPrint

import Applicative
import Int
import qualified Path
import ST
import Type.Graphic

data Permission = M | I | G | O | R deriving Show

toScopedEffect :: Permission -> ScopedEffect
toScopedEffect = soft . Foreground . \ case
  M -> Color.White
  I -> Color.Yellow
  G -> Color.Green
  O -> Color.Magenta
  R -> Color.Red

type Permissions = IntMap Permission

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

getPermissions :: MonadST m => Type (World m) a -> m Permissions
getPermissions (t_n, t_p, t_bf) = liftST $ do
  downs <- flip execStateT mempty $ forNode_' t_n $ \ (Node (toInt -> x) _) ->
    case Path.uncons =<< Map.lookup x t_p of
      Nothing -> modify $ Map.insert x Green
      Just (y, _) -> modify . Map.insert x =<< (t_bf!x,) <$> gets (!y) <$$> \ case
        (Flexible, Green) -> Green
        (Rigid, _) -> Orange
        (Flexible, Orange) -> Red
        (Flexible, Red) -> Red
  ups <- flip execStateT mempty $ forNode_ t_n $ \ (Node (toInt -> x) c) -> do
    modify $ Map.alter (\ case
      _ | poly c -> Just Polymorphic
      Nothing -> Just Monomorphic
      Just up -> Just up) x
    case Path.uncons =<< Map.lookup x t_p of
      Nothing -> return ()
      Just (y, _) ->
        modify . Map.insertWith (<>) y =<<
        (t_bf!x,) <$> gets (!x) <$$> \ case
          (_, Inert) -> Inert
          (_, Monomorphic) -> Monomorphic
          (Rigid, _) -> Inert
          _ -> Polymorphic
  return $ Map.intersectionWith (fromUp . fromDown) downs ups

poly :: Term s a -> Bool
poly = \ case
  Bot -> True
  _ -> False
