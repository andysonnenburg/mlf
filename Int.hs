module Int (IsInt (..)) where

class IsInt a where
  toInt :: a -> Int
