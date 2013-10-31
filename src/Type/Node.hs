{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TypeFamilies
  , UndecidableInstances #-}
module Type.Node
       ( Node
       , newNode
       , project
       , projected
       , preorder
       , preordered
       , postorder
       , postordered
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict

import Data.Foldable (Foldable, foldlM, foldrM)
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.IntSet as Set
import Data.Monoid (mempty)

import Prelude hiding (read)

import Int
import ST
import Supply
import UnionFind

data Node s f = Node {-# UNPACK #-} !Int (f (Set s (Node s f)))
deriving instance Show (f (Set s (Node s f))) => Show (Node s f)

instance Eq (Node s f) where
  x == y = x^.int == y^.int
  x /= y = x^.int /= y^.int

instance Hashable (Node s f) where
  hashWithSalt x = hashWithSalt x . toInt

instance IsInt (Node s f) where
  toInt (Node x _) = x

newNode :: MonadSupply Int m => f (Set s (Node s f)) -> m (Node s f)
newNode f = Node <$> supply <*> pure f

project :: Node s f -> f (Set s (Node s f))
project (Node _ x) = x

projected :: IndexPreservingGetter (Node s f) (f (Set s (Node s f)))
projected = to project

preorder :: (MonadST m, s ~ World m, Foldable f)
         => Node s f -> m [Set s (Node s f)]
preorder (Node x0 f0) = evalStateT (fix (\ rec -> foldrM $ \ s ss ->
  s^!ref.contents >>= \ (Node x f) -> use (contains x) >>= \ case
    True -> return ss
    False -> do
      contains x .= True
      (s <|) <$> rec ss f) mempty f0) $ Set.singleton x0

preordered :: (MonadST m, s ~ World m, Foldable f)
           => IndexPreservingAction m (Node s f) [Set s (Node s f)]
preordered = act preorder

postorder :: (MonadST m, s ~ World m, Foldable f)
          => Node s f -> m [Set s (Node s f)]
postorder (Node x0 f0) = evalStateT (fix (\ rec -> foldlM $ \ ss s ->
  s^!ref.contents >>= \ (Node x f) -> use (contains x) >>= \ case
    True -> return ss
    False -> do
      contains x .= True
      rec (s <| ss) f) mempty f0) $ Set.singleton x0

postordered :: (MonadST m, s ~ World m, Foldable f)
            => IndexPreservingAction m (Node s f) [Set s (Node s f)]
postordered = act postorder
