{-# LANGUAGE FlexibleContexts #-}
module Scope (scope) where

import Data.Text (Text)

import Supply
import Type.Syntactic
import Var

scope :: MonadSupply (Var a) m => PolyType Text -> m (PolyType (Var a))
scope = undefined
