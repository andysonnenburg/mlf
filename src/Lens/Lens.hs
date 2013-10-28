{-# LANGUAGE Rank2Types#-}
module Lens.Lens
       ( Lens
       , Getter
       , lask
       , (^.)
       , lget
       , lput
       , lmodify
       , lmap
       , lset
       , Prism
       , prism
       , isn't
       ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Data.Functor.Identity

infixl 8 ^.

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Getter s a = forall r . (a -> Const r a) -> s -> Const r s
type Prism s t a b = forall f . Applicative f => (a -> f b) -> s -> f t

lask :: MonadReader s m => Getter s a -> m a
{-# INLINE lask #-}
lask f = asks (getConst . f Const)

(^.) :: s -> Getter s a -> a
{-# INLINE (^.) #-}
s ^. f = getConst $ f Const s

lget :: MonadState s m => Getter s a -> m a
{-# INLINE lget #-}
lget f = gets (getConst . f Const)

lput :: MonadState s m => Lens s s a a -> a -> m ()
{-# INLINE lput #-}
lput f = modify . lset f

lmodify :: MonadState s m => Lens s s a a -> (a -> a) -> m ()
{-# INLINE lmodify #-}
lmodify f = modify . lmap f

lmap :: Lens s t a b -> (a -> b) -> s -> t
{-# INLINE lmap #-}
lmap f g = runIdentity . f (Identity . g)

lset :: Lens s t a b -> b -> s -> t
{-# INLINE lset #-}
lset f = lmap f . const

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
{-# INLINE prism #-}
prism to from = (either pure (fmap to) .) . (. from) . fmap

isn't :: Prism s t a b -> s -> Bool
{-# INLINE isn't #-}
isn't f s = case f Left s of
  Left _ -> False
  Right _ -> True
