{-# LANGUAGE LambdaCase #-}
module Type.BindingFlag (BindingFlag (..)) where

import Text.PrettyPrint.Free

data BindingFlag = Flexible | Rigid deriving (Show, Eq)

instance Pretty BindingFlag where
  pretty = \ case
    Flexible -> char '>'
    Rigid -> char '='
