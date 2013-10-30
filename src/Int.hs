module Int (IsInt (..), int) where

import Control.Lens

class IsInt a where
  toInt :: a -> Int

instance IsInt Int where
  toInt = id

int :: IsInt a => IndexPreservingGetter a Int
int = to toInt
