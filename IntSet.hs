module IntSet
       ( for_
       ) where

import Control.Applicative

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

for_ :: Applicative f => IntSet -> (Int -> f b) -> f ()
for_ xs f = IntSet.foldr (\ x m -> f x *> m) (pure ()) xs
