module Var (Var (..)) where

import Data.Text (Text)

data Var a = Var a (Maybe Text) deriving Show
