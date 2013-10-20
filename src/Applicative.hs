module Applicative
       ( (<$$>)
       , zipA2
       ) where

import Control.Applicative

infixl 4 <$$>

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap

zipA2 :: Applicative f => f a -> f b -> f (a, b)
zipA2 a b = (,) <$> a <*> b
