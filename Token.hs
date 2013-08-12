module Token (Token (..)) where

import Data.Text (Text)

data Token
  = Forall
  | Arrow
  | Bottom
  | Flexible
  | Rigid
  | LeftParen
  | RightParen
  | Var Text
  | EOF deriving Show
