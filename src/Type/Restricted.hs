{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , ViewPatterns #-}
module Type.Restricted
       ( module Type.BindingFlag
       , Type (..)
       , var
       , arr
       , bot
       , forall
       , fromSyntactic
       ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Comonad
import Control.Lens

import Data.Foldable
import Data.Function (fix)

import GHC.Generics (Generic)

import Name
import Type.BindingFlag
import Type.Syntactic (PolyType)
import qualified Type.Syntactic as S
import Supply

data Type a
  = Var a
  | Arr a a
  | Bot
  | Forall a BindingFlag (Type a) (Type a)
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance VariantA (Type a) (Type a) a a
instance VariantB (Type a) (Type a) (a, a) (a, a)
instance VariantC (Type a) (Type a) () ()
instance VariantD
         (Type a) (Type a)
         (a, BindingFlag, Type a, Type a) (a, BindingFlag, Type a, Type a)

var :: Prism' (Type a) a
var = _A

arr :: Prism' (Type a) (a, a)
arr = _B

bot :: Prism' (Type a) ()
bot = _C

forall :: Prism' (Type a) (a, BindingFlag, Type a, Type a)
forall = _D

fromSyntactic :: (Comonad w, MonadSupply Int m)
              => w (PolyType w (Name a)) -> m (Type (Name a))
fromSyntactic = fromPoly
  where
    fromPoly = fix $ \ rec -> extract >>> \ case
      S.Mono t -> fromMono t
      S.Bot -> return Bot
      S.Forall x bf a b -> Forall x bf <$> rec a <*> rec b
    fromMono = fix $ \ rec -> \ case
      S.Var x -> return $ Var x
      S.Arr (extract -> t) (extract -> u) -> do
        a <- supplyName
        b <- supplyName
        Forall a Flexible <$> rec t <*> (Forall b Flexible <$> rec u <*> pure (Arr a b))
