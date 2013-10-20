{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor #-}
module IntSet
       ( IntSet
       , singleton
       , insert
       , member
       , notMember
       ) where

import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Semigroup

import Int

newtype IntSet a = IntSet { unIntSet :: IntMap a
                          } deriving ( Functor
                                     , Foldable
                                     )

instance IsInt a => Semigroup (IntSet a) where
  x <> y = IntSet $ unIntSet x <> unIntSet y

instance IsInt a => Monoid (IntSet a) where
  mempty = IntSet IntMap.empty
  mappend x y = IntSet $ mappend (unIntSet x) (unIntSet y)
  mconcat = IntSet . mconcat . fmap unIntSet

singleton :: IsInt a => a -> IntSet a
singleton a = IntSet $ IntMap.singleton (toInt a) a

insert :: IsInt a => a -> IntSet a -> IntSet a
insert a = IntSet . IntMap.insert (toInt a) a . unIntSet

member :: IsInt a => a -> IntSet a -> Bool
member a = IntMap.member (toInt a) . unIntSet

notMember :: IsInt a => a -> IntSet a -> Bool
notMember a = IntMap.notMember (toInt a) . unIntSet
