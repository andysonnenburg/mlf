{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Product (Product (..), local) where

import Control.Comonad.Env (Comonad (..),
                            ComonadApply (..),
                            ComonadEnv (..))

import Data.Foldable
import Data.Semigroup (Semigroup)
import Data.Traversable

import GHC.Generics (Generic)

import System.Console.Terminfo.PrettyPrint

import Text.PrettyPrint.Free

import Lens

data Product a b = a :* b deriving ( Bounded
                                   , Eq
                                   , Ord
                                   , Read
                                   , Show
                                   , Functor
                                   , Foldable
                                   , Generic
                                   , Traversable
                                   )

instance Comonad (Product e) where
  {-# INLINE duplicate #-}
  duplicate p@(e :* _) = e :* p
  {-# INLINE extract #-}
  extract (_ :* a) = a

instance Semigroup e => ComonadApply (Product e) where
  {-# INLINE (<@>) #-}
  (e :* f) <@> (e' :* a) = (e <> e') :* f a
  {-# INLINE (<@) #-}
  (e :* a) <@ (e' :* _) = (e <> e') :* a
  {-# INLINE (@>) #-}
  (e :* _) @> (e' :* b) = (e <> e') :* b

instance ComonadEnv e (Product e) where
  {-# INLINE ask #-}
  ask (e :* _) = e

instance Pretty b => Pretty (Product a b) where
  {-# INLINE pretty #-}
  pretty (_ :* b) = pretty b

instance PrettyTerm b => PrettyTerm (Product ScopedEffect b) where
  prettyTerm (e :* b) = with e $ prettyTerm b

instance Field1 (Product a b) (Product a' b) a a'
instance Field2 (Product a b) (Product a b') b b'

local :: (e -> e') -> Product e a -> Product e' a
local f (e :* a) = f e :* a
