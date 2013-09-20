{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Permission
       ( Permission (..)
       , Permissions
       , getPermissions
       ) where

import Control.Applicative
import Control.Monad.ST.Safe
import Control.Monad.State.Strict

import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import Data.Semigroup

import Applicative
import Int
import qualified Path
import Type.Graphic

data Permission = M | I | G | O | R deriving Show

type Permissions = IntMap Permission

data Down = Green | Orange | Red

fromDown :: Down -> Permission
fromDown = \ case
  Green -> G
  Orange -> O
  Red -> R

data Up = Monomorphic | Inert | Neither

instance Semigroup Up where
  Neither <> _ = Neither
  _ <> Neither = Neither
  Inert <> _ = Inert
  _ <> Inert = Inert
  Monomorphic <> Monomorphic = Monomorphic

fromUp :: Permission -> Up -> Permission
fromUp x = \ case
  Monomorphic -> M
  Inert -> I
  Neither -> x

getPermissions :: Type s a -> ST s Permissions
getPermissions (t_n, t_p, t_bf) = do
  ups <- flip execStateT mempty $ forNode_ t_n $ \ (Node (toInt -> x) c) -> do
    modify $ Map.alter (\ case
      _ | poly c -> Just Neither
      Nothing -> Just Monomorphic
      Just up -> Just up) x
    case Path.uncons =<< Map.lookup x t_p of
      Nothing -> return ()
      Just (y, _) ->
        modify . Map.insertWith (<>) y =<<
        (,) (t_bf!x) <$> gets (!x) <$$> \ case
          (_, Inert) -> Inert
          (_, Monomorphic) -> Monomorphic
          (Rigid, _) -> Inert
          _ -> Neither
  downs <- flip execStateT mempty $ forNode_' t_n $ \ (Node (toInt -> x) _) ->
    case Path.uncons =<< Map.lookup x t_p of
      Nothing -> modify $ Map.insert x Green
      Just (y, _) -> modify . Map.insert x =<< (,) (t_bf!x) <$> gets (!y) <$$> \ case
        (Flexible, Green) -> Green
        (Rigid, _) -> Orange
        (Flexible, Orange) -> Red
        (Flexible, Red) -> Red
  return $ Map.intersectionWith (flip $ fromUp . fromDown) ups downs

poly :: Term s a -> Bool
poly = \ case
  Bot -> True
  _ -> False
