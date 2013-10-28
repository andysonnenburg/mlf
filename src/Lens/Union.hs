{-# LANGUAGE
    CPP
  , DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , Rank2Types
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances #-}
module Lens.Union
       ( VariantA (..)
       , VariantB (..)
       ) where

import Control.Applicative

import Data.Proxy (Proxy (Proxy))

import GHC.Generics (Generic (..), (:+:) (..), (:*:) (..), K1 (..), M1 (..), U1 (..))

import Lens.Lens

class VariantA s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _A :: Prism s t a b
#ifndef HLINT
  default _A :: (Generic s, Generic t, GIxed N0 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _A #-}
  _A = ix (Proxy :: Proxy N0)
#endif

class VariantB s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _B :: Prism s t a b
#ifndef HLINT
  default _B :: (Generic s, Generic t, GIxed N1 (Rep s) (Rep t) a b)
             => Prism s t a b
  {-# INLINE _B #-}
  _B = ix (Proxy :: Proxy N1)
#endif

ix :: (Generic s, Generic t, GIxed n (Rep s) (Rep t) a b)
   => f n -> Prism s t a b
{-# INLINE ix #-}
ix n = \ f -> fmap to . gix n f . from

#ifndef HLINT
class GIxed (n :: Nat) s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  gix :: f n -> Prism (s x) (t x) a b
#endif

instance GIxed N0 (K1 i a) (K1 i b) a b where
  {-# INLINE gix #-}
  gix _ = prism K1 (Right . unK1)

instance GIxed n s t a b => GIxed n (M1 i c s) (M1 i c t) a b where
  {-# INLINE gix #-}
  gix n = \ f -> fmap M1 . gix n f . unM1

instance GIxed' (GSize s > n) n s s' t t' a b
      => GIxed n (s :+: s') (t :+: t') a b where
  {-# INLINE gix #-}
  gix n = \ f s -> gix' (reproxySizeGT (proxyL s) n) n f s

instance (IsGTuple s, IsGTuple s', IsGTuple t, IsGTuple t',
          IsTuple (GList (s :*: s')), IsTuple (GList (t :*: t')),
          a ~ ToTuple (s :*: s'), b ~ ToTuple (t :*: t'))
      => GIxed N0 (s :*: s') (t :*: t') a b where
  {-# INLINE gix #-}
  gix _ = \ f -> fmap (fromGTuple . fromTuple) . f . toTuple . toGTuple

#ifndef HLINT
class GIxed' (p :: Bool) (n :: Nat) s s' t t' a b where
  gix' :: f p -> g n -> Prism ((s :+: s') x) ((t :+: t') x) a b
#endif

instance (GIxed n s t a b, s' ~ t') => GIxed' True n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ n = \ f -> gsum (fmap L1 . gix n f) (pure . R1)

instance (GIxed (Subtract (GSize s) n) s' t' a b, s ~ t)
      => GIxed' False n s s' t t' a b where
  {-# INLINE gix' #-}
  gix' _ n = \ f s ->
    gsum (pure . L1) (fmap R1 . gix (reproxySubtractSize (proxyL s) n) f) s

#ifndef HLINT
data GTuple xs where
  U :: GTuple '[]
  (:*) :: x -> GTuple xs -> GTuple (x ': xs)
#endif

infixr 5 :*

#ifndef HLINT
uncons :: (a -> GTuple as -> r) -> GTuple (a ': as) -> r
{-# INLINE uncons #-}
uncons f (a :* as) = f a as
#endif

#ifndef HLINT
unnil :: r -> GTuple '[] -> r
{-# INLINE unnil #-}
unnil r U = r
#endif

class IsTuple xs where
  type Tuple xs
  toTuple :: GTuple xs -> Tuple xs
  fromTuple :: Tuple xs -> GTuple xs

#ifndef HLINT
instance IsTuple '[] where
  type Tuple '[] = ()
  {-# INLINE toTuple #-}
  toTuple _ = ()
  {-# INLINE fromTuple #-}
  fromTuple _ = U
#endif

#ifndef HLINT
instance IsTuple [a, b] where
  type Tuple [a, b] = (a, b)
  {-# INLINE toTuple #-}
  toTuple = uncons $ \ a -> uncons $ \ b -> unnil (a, b)
  {-# INLINE fromTuple #-}
  fromTuple (a, b) = a :* b :* U
#endif

type ToTuple s = Tuple (GList s)

class IsGTuple s where
  type GCons s xs
  gcons :: s x -> GTuple xs -> GTuple (GCons s xs)
  guncons :: (s x -> GTuple xs -> r) -> GTuple (GCons s xs) -> r

#ifndef HLINT
type GList s = GCons s '[]
#endif

toGTuple :: IsGTuple s => s x -> GTuple (GList s)
{-# INLINE toGTuple #-}
toGTuple = flip gcons U

fromGTuple :: IsGTuple s => GTuple (GList s) -> s x
{-# INLINE fromGTuple #-}
fromGTuple = guncons unnil

instance IsGTuple U1 where
  type GCons U1 xs = xs
  {-# INLINE gcons #-}
  gcons = flip const
  {-# INLINE guncons #-}
  guncons = ($ U1)

#ifndef HLINT
instance IsGTuple (K1 i c) where
  type GCons (K1 i c) xs = c ': xs
  {-# INLINE gcons #-}
  gcons = (:*) . unK1
  {-# INLINE guncons #-}
  guncons f = uncons $ \ c ys -> f (K1 c) ys
#endif

instance IsGTuple f => IsGTuple (M1 i c f) where
  type GCons (M1 i c f) xs = GCons f xs
  {-# INLINE gcons #-}
  gcons = gcons . unM1
  {-# INLINE guncons #-}
  guncons f = guncons $ f . M1

instance (IsGTuple a, IsGTuple b) => IsGTuple (a :*: b) where
  type GCons (a :*: b) xs = GCons a (GCons b xs)
  {-# INLINE gcons #-}
  gcons (a :*: b) = gcons a . gcons b
  {-# INLINE guncons #-}
  guncons f = guncons $ \ a -> guncons $ \ b -> f $ a :*: b

gsum :: (a x -> r) -> (b x -> r) -> (a :+: b) x -> r
{-# INLINE gsum #-}
gsum f _ (L1 a) = f a
gsum _ f (R1 a) = f a

#ifndef HLINT
type family GSize (f :: * -> *) :: Nat
#endif
type instance GSize U1 = Z
type instance GSize (K1 i c) = S Z
type instance GSize (M1 i c f) = GSize f
type instance GSize (a :+: b) = GSize a + GSize b
type instance GSize (a :*: b) = S Z

reproxySubtractSize :: f (s x) -> g n -> Proxy (Subtract (GSize s) n)
{-# INLINE reproxySubtractSize #-}
reproxySubtractSize _ _ = Proxy

reproxySizeGT :: f (s x) -> g n -> Proxy (GSize s > n)
{-# INLINE reproxySizeGT #-}
reproxySizeGT _ _ = Proxy

proxyL :: (a :+: b) x -> Proxy (a x)
{-# INLINE proxyL #-}
proxyL _ = Proxy

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
