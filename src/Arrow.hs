module Arrow
       ( WrappedApplicative (..)
       , applyArr
       , runArr
       , liftArr
       ) where

import Control.Applicative
import Control.Arrow
import Control.Category

import Prelude hiding ((.), id)

newtype WrappedApplicative f a b =
  WrapApplicative { unwrapApplicative :: f (a -> b) }

applyArr :: Functor f => WrappedApplicative f a b -> a -> f b
applyArr f a = fmap ($ a) $ unwrapApplicative f

runArr :: Functor f => WrappedApplicative f () a -> f a
runArr = fmap ($ ()) . unwrapApplicative

liftArr :: Functor f => f b -> WrappedApplicative f a b
liftArr = WrapApplicative . fmap const

instance Applicative f => Category (WrappedApplicative f) where
  id = WrapApplicative $ pure id
  g . f = WrapApplicative $ (>>>) <$> unwrapApplicative f <*> unwrapApplicative g

instance Applicative f => Arrow (WrappedApplicative f) where
  arr = WrapApplicative . pure
  first = WrapApplicative . fmap first . unwrapApplicative

instance Alternative f => ArrowZero (WrappedApplicative f) where
  zeroArrow = WrapApplicative empty

instance Alternative f => ArrowPlus (WrappedApplicative f) where
  f <+> g = WrapApplicative $ unwrapApplicative f <|> unwrapApplicative g
