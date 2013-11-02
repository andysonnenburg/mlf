{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , TypeFamilies #-}
module IntSet
       ( IntSet
       , singleton
       , insert
       , member
       , notMember
       , delete
       , intersection
       ) where

import Control.Lens

import Data.Constraint
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

type instance Index (IntSet a) = a

type instance IxValue (IntSet a) = a

instance IsInt a => Contains (IntSet a) where
  type Containing (IntSet a) f = Functor f
  contains k f s = f (member k s) <&> \ b ->
    if b then insert k s else delete k s
  {-# INLINE contains #-}
  containsProof _ _ = Sub Dict

singleton :: IsInt a => a -> IntSet a
singleton a = IntSet $ IntMap.singleton (toInt a) a

insert :: IsInt a => a -> IntSet a -> IntSet a
insert a = IntSet . IntMap.insert (toInt a) a . unIntSet

member :: IsInt a => a -> IntSet a -> Bool
member a = IntMap.member (toInt a) . unIntSet

notMember :: IsInt a => a -> IntSet a -> Bool
notMember a = IntMap.notMember (toInt a) . unIntSet

delete :: IsInt a => a -> IntSet a -> IntSet a
delete a = IntSet . IntMap.delete (toInt a) . unIntSet

intersection :: IsInt a => IntSet a -> IntSet b -> IntSet a
intersection xs ys = IntSet $ IntMap.intersection (unIntSet xs) (unIntSet ys)
