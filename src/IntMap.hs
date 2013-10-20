{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable #-}
module IntMap
       ( IntMap
       , (!)
       , insert
       , insertWith
       , delete
       , lookup
       , member
       , notMember
       , alter
       , intersectionWith
       , toIntMap
       ) where

import Data.Foldable
import qualified Data.IntMap.Strict as Internal
import Data.Semigroup (Monoid (..), Semigroup (..))
import Data.Traversable

import Prelude hiding (lookup)

import Int

newtype IntMap k v = IntMap { unIntMap :: Internal.IntMap v
                            } deriving ( Functor
                                       , Foldable
                                       , Traversable
                                       )

instance Semigroup (IntMap k v) where
  x <> y = IntMap $ unIntMap x <> unIntMap y

instance Monoid (IntMap k v) where
  mempty = IntMap mempty
  mappend x y = IntMap $ mappend (unIntMap x) (unIntMap y)
  mconcat = IntMap . mconcat . fmap unIntMap

(!) :: IsInt k => IntMap k v -> k -> v
m!k = unIntMap m Internal.! toInt k

insert :: IsInt k => k -> v -> IntMap k v -> IntMap k v
insert k v = IntMap . Internal.insert (toInt k) v . unIntMap

insertWith :: IsInt k => (v -> v -> v) -> k -> v -> IntMap k v -> IntMap k v
insertWith f k v = IntMap . Internal.insertWith f (toInt k) v . unIntMap

delete :: IsInt k => k -> IntMap k v -> IntMap k v
delete k = IntMap . Internal.delete (toInt k) . unIntMap

lookup :: IsInt k => k -> IntMap k v -> Maybe v
lookup k = Internal.lookup (toInt k) . unIntMap

member :: IsInt k => k -> IntMap k v -> Bool
member k = Internal.member (toInt k) . unIntMap

notMember :: IsInt k => k -> IntMap k v -> Bool
notMember k = Internal.notMember (toInt k) . unIntMap

alter :: IsInt k => (Maybe v -> Maybe v) -> k -> IntMap k v -> IntMap k v
alter f k = IntMap . Internal.alter f (toInt k) . unIntMap

intersectionWith :: (a -> b -> c) -> IntMap k a -> IntMap k b -> IntMap k c
intersectionWith f as bs =
  IntMap $ Internal.intersectionWith f (unIntMap as) (unIntMap bs)

toIntMap :: IntMap k v -> Internal.IntMap v
toIntMap = unIntMap
