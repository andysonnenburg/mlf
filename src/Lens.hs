{-# LANGUAGE
    CPP
  , DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies #-}
#ifndef HLINT
{-# LANGUAGE KindSignatures #-}
#endif
{-# LANGUAGE
    MultiParamTypeClasses
  , Rank2Types
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances #-}
module Lens
       ( Lens
       , Getter
       , lask
       , (^.)
       , lget
       , lput
       , lmodify
       , lmap
       , lset
       , Field1 (..)
       , Field2 (..)
       , Field3 (..)
       , Field4 (..)
       , Field5 (..)
       , Field6 (..)
       , Field7 (..)
       , Field8 (..)
       , Field9 (..)
       ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Data.Functor.Identity
import Data.Proxy (Proxy (Proxy))

import GHC.Generics (Generic (..), (:*:) (..), K1 (..), M1 (..), U1 (..))

infixl 8 ^.

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Getter s a = forall r . (a -> Const r a) -> s -> Const r s

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

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: Lens s t a b
#ifndef HLINT
  default _1 :: (Generic s, Generic t, GIxed N0 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _1 #-}
  _1 = ix proxyN0
#endif

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b
#ifndef HLINT
  default _2 :: (Generic s, Generic t, GIxed N1 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _2 #-}
  _2 = ix proxyN1
#endif

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b
#ifndef HLINT
  default _3 :: (Generic s, Generic t, GIxed N2 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _3 #-}
  _3 = ix proxyN2
#endif

class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _4 :: Lens s t a b
#ifndef HLINT
  default _4 :: (Generic s, Generic t, GIxed N3 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _4 #-}
  _4 = ix proxyN3
#endif

class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _5 :: Lens s t a b
#ifndef HLINT
  default _5 :: (Generic s, Generic t, GIxed N4 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _5 #-}
  _5 = ix proxyN4
#endif

class Field6 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _6 :: Lens s t a b
#ifndef HLINT
  default _6 :: (Generic s, Generic t, GIxed N5 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _6 #-}
  _6 = ix proxyN5
#endif

class Field7 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _7 :: Lens s t a b
#ifndef HLINT
  default _7 :: (Generic s, Generic t, GIxed N6 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _7 #-}
  _7 = ix proxyN6
#endif

class Field8 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _8 :: Lens s t a b
#ifndef HLINT
  default _8 :: (Generic s, Generic t, GIxed N7 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _8 #-}
  _8 = ix proxyN7
#endif

class Field9 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _9 :: Lens s t a b
#ifndef HLINT
  default _9 :: (Generic s, Generic t, GIxed N8 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _9 #-}
  _9 = ix proxyN8
#endif

instance Field1 (a, b) (a', b) a a'
instance Field1 (a, b, c) (a', b, c) a a'
instance Field1 (a, b, c, d) (a', b, c, d) a a'
instance Field1 (a, b, c, d, e) (a', b, c, d, e) a a'
instance Field1 (a, b, c, d, e, f) (a', b, c, d, e, f) a a'
instance Field1 (a, b, c, d, e, f, g) (a', b, c, d, e, f, g) a a'

instance Field2 (a, b) (a, b') b b'
instance Field2 (a, b, c) (a, b', c) b b'
instance Field2 (a, b, c, d) (a, b', c, d) b b'
instance Field2 (a, b, c, d, e) (a, b', c, d, e) b b'
instance Field2 (a, b, c, d, e, f) (a, b', c, d, e, f) b b'
instance Field2 (a, b, c, d, e, f, g) (a, b', c, d, e, f, g) b b'

instance Field3 (a, b, c) (a, b, c') c c'
instance Field3 (a, b, c, d) (a, b, c', d) c c'
instance Field3 (a, b, c, d, e) (a, b, c', d, e) c c'
instance Field3 (a, b, c, d, e, f) (a, b, c', d, e, f) c c'
instance Field3 (a, b, c, d, e, f, g) (a, b, c', d, e, f, g) c c'

instance Field4 (a, b, c, d) (a, b, c, d') d d'
instance Field4 (a, b, c, d, e) (a, b, c, d', e) d d'
instance Field4 (a, b, c, d, e, f) (a, b, c, d', e, f) d d'
instance Field4 (a, b, c, d, e, f, g) (a, b, c, d', e, f, g) d d'

instance Field5 (a, b, c, d, e) (a, b, c, d, e') e e'
instance Field5 (a, b, c, d, e, f) (a, b, c, d, e', f) e e'
instance Field5 (a, b, c, d, e, f, g) (a, b, c, d, e', f, g) e e'

instance Field6 (a, b, c, d, e, f) (a, b, c, d, e, f') f f'
instance Field6 (a, b, c, d, e, f, g) (a, b, c, d, e, f', g) f f'

instance Field7 (a, b, c, d, e, f, g) (a, b, c, d, e, f, g') g g'

ix :: (Generic s, Generic t, GIxed n (Rep s) (Rep t) a b) => f n -> Lens s t a b
{-# INLINE ix #-}
ix n = \ f -> fmap to . gix n f . from

#ifndef HLINT
class GIxed (n :: Nat) s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  gix :: f n -> Lens (s x) (t x) a b
#endif

instance GIxed N0 (K1 i a) (K1 i b) a b where
  {-# INLINE gix #-}
  gix _ = \ f -> fmap K1 . f . unK1

instance GIxed n s t a b => GIxed n (M1 i c s) (M1 i c t) a b where
  {-# INLINE gix #-}
  gix n = \ f -> fmap M1 . gix n f . unM1

instance GIxed' (GSize s > n) n s s' t t' a b
      => GIxed n (s :*: s') (t :*: t') a b where
  {-# INLINE gix #-}
  gix n = \ f s -> gix' (proxySizeGT (fst' s) n) n f s

#ifndef HLINT
class GIxed' (p :: Bool) (n :: Nat) s s' t t' a b where
  gix' :: f p -> g n -> Lens ((s :*: s') x) ((t :*: t') x) a b
#endif

instance (GIxed n s t a b, s' ~ t') => GIxed' True n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ n = \ f (s :*: s') -> fmap (:*: s') $ gix n f s

instance (GIxed (Subtract (GSize s) n) s' t' a b, s ~ t)
      => GIxed' False n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ n = \ f (s :*: s') -> fmap (s :*:) $ gix (proxySubtractSize s n) f s'

#ifndef HLINT
type family GSize (f :: * -> *) :: Nat
#endif
type instance GSize U1 = Z
type instance GSize (K1 i c) = S Z
type instance GSize (M1 i c f) = GSize f
type instance GSize (a :*: b) = GSize a + GSize b

proxySubtractSize :: s x -> f n -> Proxy (Subtract (GSize s) n)
{-# INLINE proxySubtractSize #-}
proxySubtractSize _ _ = Proxy

proxySizeGT :: s x -> f n -> Proxy (GSize s > n)
{-# INLINE proxySizeGT #-}
proxySizeGT _ _ = Proxy

fst' :: (a :*: b) x -> a x
{-# INLINE fst' #-}
fst' (a :*: _) = a

data Nat = Z | S Nat

#ifndef HLINT
type family (x :: Nat) + (y :: Nat) :: Nat
#endif
type instance Z + y = y
type instance S x + y = S (x + y)

#ifndef HLINT
type family Subtract (x :: Nat) (y :: Nat) :: Nat
#endif
type instance Subtract Z x = x
type instance Subtract (S x) (S y) = Subtract x y

#ifndef HLINT
type family (x :: Nat) > (y :: Nat) :: Bool
#endif
type instance Z > x = False
type instance S x > Z = True
type instance S x > S y = x > y

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7

proxyN0 :: Proxy N0
{-# INLINE proxyN0 #-}
proxyN0 = Proxy

proxyN1 :: Proxy N1
{-# INLINE proxyN1 #-}
proxyN1 = Proxy

proxyN2 :: Proxy N2
{-# INLINE proxyN2 #-}
proxyN2 = Proxy

proxyN3 :: Proxy N3
{-# INLINE proxyN3 #-}
proxyN3 = Proxy

proxyN4 :: Proxy N4
{-# INLINE proxyN4 #-}
proxyN4 = Proxy

proxyN5 :: Proxy N5
{-# INLINE proxyN5 #-}
proxyN5 = Proxy

proxyN6 :: Proxy N6
{-# INLINE proxyN6 #-}
proxyN6 = Proxy

proxyN7 :: Proxy N7
{-# INLINE proxyN7 #-}
proxyN7 = Proxy

proxyN8 :: Proxy N8
{-# INLINE proxyN8 #-}
proxyN8 = Proxy
