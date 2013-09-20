module Applicative
       ( (<$$>)
       ) where

infixl 4 <$$>

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
