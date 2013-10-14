{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , KindSignatures
  , MultiParamTypeClasses
  , Rank2Types
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances #-}
module Lens
       ( Lens
       , lget
       , lput
       , lmodify
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

import Data.Functor.Identity
import Data.Proxy (Proxy (Proxy))

import GHC.Generics (Generic (..), (:*:) (..), K1 (..), M1 (..), U1 (..))

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

lget :: Lens s s a a -> s -> a
{-# INLINE lget #-}
lget f = getConst . f Const

lput :: Lens s t a b -> b -> s -> t
{-# INLINE lput #-}
lput f = lmodify f . const

lmodify :: Lens s t a b -> (a -> b) -> s -> t
{-# INLINE lmodify #-}
lmodify f g = runIdentity . f (Identity . g)

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: Lens s t a b
  default _1 :: (Generic s, Generic t, GAt N0 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _1 #-}
  _1 = at proxyN0

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b
  default _2 :: (Generic s, Generic t, GAt N1 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _2 #-}
  _2 = at proxyN1

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b
  default _3 :: (Generic s, Generic t, GAt N2 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _3 #-}
  _3 = at proxyN2

class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _4 :: Lens s t a b
  default _4 :: (Generic s, Generic t, GAt N3 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _4 #-}
  _4 = at proxyN3

class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _5 :: Lens s t a b
  default _5 :: (Generic s, Generic t, GAt N4 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _5 #-}
  _5 = at proxyN4

class Field6 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _6 :: Lens s t a b
  default _6 :: (Generic s, Generic t, GAt N5 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _6 #-}
  _6 = at proxyN5

class Field7 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _7 :: Lens s t a b
  default _7 :: (Generic s, Generic t, GAt N6 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _7 #-}
  _7 = at proxyN6

class Field8 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _8 :: Lens s t a b
  default _8 :: (Generic s, Generic t, GAt N7 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _8 #-}
  _8 = at proxyN7

class Field9 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _9 :: Lens s t a b
  default _9 :: (Generic s, Generic t, GAt N8 (Rep s) (Rep t) a b)
             => Lens s t a b
  {-# INLINE _9 #-}
  _9 = at proxyN8

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

at :: (Generic s, Generic t, GAt n (Rep s) (Rep t) a b) => f n -> Lens s t a b
{-# INLINE at #-}
at n = \ f -> fmap to . gat n f . from

class GAt (n :: Nat) s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  gat :: f n -> Lens (s x) (t x) a b

instance GAt N0 (K1 i a) (K1 i b) a b where
  {-# INLINE gat #-}
  gat _ = \ f -> fmap K1 . f . unK1

instance GAt n s t a b => GAt n (M1 i c s) (M1 i c t) a b where
  {-# INLINE gat #-}
  gat n = \ f -> fmap M1 . gat n f . unM1

instance GAt' (GSize s > n) n s s' t t' a b
      => GAt n (s :*: s') (t :*: t') a b where
  {-# INLINE gat #-}
  gat n = \ f s -> gat' (proxySizeGT (fst' s) n) n f s

class GAt' (p :: Bool) (n :: Nat) s s' t t' a b where
  gat' :: f p -> g n -> Lens ((s :*: s') x) ((t :*: t') x) a b

instance (GAt n s t a b, s' ~ t') => GAt' True n s s' t t' a b where
  {-# INLINE gat' #-}
  gat' _ n = \ f (s :*: s') -> fmap (:*: s') $ gat n f s

instance (GAt (Subtract (GSize s) n) s' t' a b, s ~ t)
      => GAt' False n s s' t t' a b where
  {-# INLINE gat' #-}
  gat' _ n = \ f (s :*: s') -> fmap (s :*:) $ gat (proxySubtractSize s n) f s'

type family GSize (f :: * -> *) :: Nat
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

type family (x :: Nat) + (y :: Nat) :: Nat
type instance Z + y = y
type instance S x + y = S (x + y)

type family Subtract (x :: Nat) (y :: Nat) :: Nat
type instance Subtract Z x = x
type instance Subtract (S x) (S y) = Subtract x y

type family (x :: Nat) > (y :: Nat) :: Bool
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
