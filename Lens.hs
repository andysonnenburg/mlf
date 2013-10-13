{-# LANGUAGE
    DataKinds
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
module Lens
       ( Lens
       , lget
       , lput
       , lmodify
       , Field1 (..)
       , Field2 (..)
       , Field3 (..)
       ) where

import Control.Applicative

import Data.Functor.Identity

import GHC.Generics

import Prelude hiding (take)

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
  default _1 :: (Generic s, GTuple (Rep s),
                 Generic t, GTuple (Rep t),
                 At N0 (GList (Rep s)) (GList (Rep t)) a b)
             => Lens s t a b
  {-# INLINE _1 #-}
  _1 f = fmap (to . gfromTuple) . at proxyN0 f . gtoTuple . from

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b
  default _2 :: (Generic s, GTuple (Rep s),
                 Generic t, GTuple (Rep t),
                 At N1 (GList (Rep s)) (GList (Rep t)) a b)
             => Lens s t a b
  {-# INLINE _2 #-}
  _2 f = fmap (to . gfromTuple) . at proxyN1 f . gtoTuple . from

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b
  default _3 :: (Generic s, GTuple (Rep s),
                 Generic t, GTuple (Rep t),
                 At N2 (GList (Rep s)) (GList (Rep t)) a b)
             => Lens s t a b
  {-# INLINE _3 #-}
  _3 f = fmap (to . gfromTuple) . at proxyN2 f . gtoTuple . from

instance Field1 (a, b) (a', b) a a'
instance Field1 (a, b, c) (a', b, c) a a'

instance Field2 (c, a) (c, b) a b
instance Field2 (c, a, d) (c, b, d) a b

instance Field3 (c, d, a) (c, d, b) a b

type GList f = GCons f '[]

gtoTuple :: GTuple f => f x -> Tuple (GList f)
{-# INLINE gtoTuple #-}
gtoTuple = flip gcons U

gfromTuple :: GTuple f => Tuple (GList f) -> f x
{-# INLINE gfromTuple #-}
gfromTuple = guncons unnil

class GTuple f where
  type GCons f xs
  gcons :: f x -> Tuple xs -> Tuple (GCons f xs)
  guncons :: (f x -> Tuple xs -> r) -> Tuple (GCons f xs) -> r

instance GTuple U1 where
  type GCons U1 xs = xs
  {-# INLINE gcons #-}
  gcons = flip const
  {-# INLINE guncons #-}
  guncons = ($ U1)

instance GTuple (K1 i x) where
  type GCons (K1 i x) xs = x ': xs
  {-# INLINE gcons #-}
  gcons = (:*) . unK1
  {-# INLINE guncons #-}
  guncons f = uncons $ f . K1

instance GTuple f => GTuple (M1 i c f) where
  type GCons (M1 i c f) xs = GCons f xs
  {-# INLINE gcons #-}
  gcons = gcons . unM1
  {-# INLINE guncons #-}
  guncons f = guncons $ f . M1

instance (GTuple a, GTuple b) => GTuple (a :*: b) where
  type GCons (a :*: b) xs = GCons a (GCons b xs)
  {-# INLINE gcons #-}
  gcons (a :*: b) = gcons a . gcons b
  {-# INLINE guncons #-}
  guncons f = guncons $ \ a -> guncons $ \ b -> f $ a :*: b

data Tuple xs where
  U :: Tuple '[]
  (:*) :: x -> !(Tuple xs) -> Tuple (x ': xs)

uncons :: (x -> Tuple xs -> r) -> Tuple (x ': xs) -> r
{-# INLINE uncons #-}
uncons f (x :* xs) = f x xs

unnil :: r -> Tuple '[] -> r
{-# INLINE unnil #-}
unnil r U = r

data Nat = Z | S Nat

class At (n :: Nat) s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  at :: p n -> Lens (Tuple s) (Tuple t) a b

instance At Z (x ': xs) (y ': xs) x y where
  {-# INLINE at #-}
  at _ = \ f (x :* xs) -> (:* xs) <$> f x

instance At n s t a b => At ('S n) (x ': s) (x ': t) a b where
  {-# INLINE at #-}
  at p = \ f (x :* xs) -> (x :*) <$> at (reproxyPred p) f xs

data Proxy (n :: Nat) = Proxy

reproxyPred :: p ('S n) -> Proxy n
{-# INLINE reproxyPred #-}
reproxyPred _ = Proxy

type N0 = Z
type N1 = 'S N0
type N2 = 'S N1

proxyN0 :: Proxy N0
{-# INLINE proxyN0 #-}
proxyN0 = Proxy

proxyN1 :: Proxy N1
{-# INLINE proxyN1 #-}
proxyN1 = Proxy

proxyN2 :: Proxy N2
{-# INLINE proxyN2 #-}
proxyN2 = Proxy
