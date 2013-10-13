{-# LANGUAGE LambdaCase #-}
module List (list) where

list :: r -> (a -> [a] -> r) -> [a] -> r
{-# INLINE list #-}
list nil cons = \ case
  [] -> nil
  x:xs -> cons x xs
