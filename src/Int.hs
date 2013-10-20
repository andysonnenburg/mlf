module Int (IsInt (..), int) where

import Control.Applicative

import Lens

class IsInt a where
  toInt :: a -> Int

instance IsInt Int where
  toInt = id

int :: IsInt a => Getter a Int
int f = Const . getConst . f . toInt
