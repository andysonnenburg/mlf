module Int (IsInt (..)) where

class IsInt a where
  toInt :: a -> Int

instance IsInt Int where
  toInt = id
