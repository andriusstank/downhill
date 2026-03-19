{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Linear.Fun (
  DualOf,
  LinearMap (..),
  SomeLinearMap (..),
  IdFun (..),
  NegateFun (..),
  ScalarMulFun (..),
  Metric,
  Fst (..),
  (|*>),
  (<*|),
  (<<*|), (|*>>),
  lflip, lfst, lsnd, lid, lzero
)
where

import Data.Data (Proxy (Proxy))
import Downhill.Orphans
import NumHask.Prelude

type family DualOf (v :: Type) :: Type

type instance DualOf Int = Int
type instance DualOf Integer = Integer
type instance DualOf Float = Float
type instance DualOf Double = Double
type instance DualOf () = ()
type instance DualOf (a, b) = (DualOf a, DualOf b)
type instance DualOf (a, b, c) = (DualOf a, DualOf b, DualOf c)
type instance DualOf (Generically v) = Generically (DualOf v)
type instance DualOf (Complex a) = Complex a

class LinearMap f a b | f a -> b, f b -> a where
  lmapFwd :: f -> a -> b
  lmapBack :: f -> DualOf b -> DualOf a

infixr 7 <*|
(<*|) :: (LinearMap f a b) => f -> a -> b
(<*|) = lmapFwd

infixl 7 |*>
(|*>) ::
  forall f da db.
  ( LinearMap f (DualOf da) (DualOf db)
  , DualOf (DualOf da) ~ da
  , DualOf (DualOf db) ~ db
  ) =>
  db -> f -> da
(|*>) = flip (lmapBack @f @(DualOf da) @(DualOf db))

class (LinearMap f a (DualOf a)) => Metric f a

data SomeLinearMap (a :: Type) (b :: Type) where
  SomeLinearMap ::
    forall f a b.
    (LinearMap f a b) =>
    f -> SomeLinearMap a b

(<<*|) :: SomeLinearMap a b -> a -> b
SomeLinearMap f <<*| x = f <*| x

(|*>>) :: (DualOf (DualOf a) ~ a, DualOf (DualOf b) ~ b) => DualOf b -> SomeLinearMap a b -> DualOf a
x |*>> SomeLinearMap f = x |*> f


{-
instance LinearMap (SomeLinearMap a b) a b where
  lmapFwd (SomeLinearMap f) = lmapFwd f
  lmapBack (SomeLinearMap f) = go f
    where go :: forall f. LinearMap f a b => f -> DualOf b -> DualOf a
          go = lmapBack @f @a @b
-}

data IdFun = IdFun

instance LinearMap IdFun a a where
  lmapFwd IdFun x = x
  lmapBack IdFun x = x

data Zero a b = Zero

instance (Additive (DualOf a), Additive b) => LinearMap (Zero a b) a b where
  lmapFwd = const zero
  lmapBack = const zero

lflip :: (DualOf (DualOf a) ~ a, DualOf (DualOf b) ~ b) => SomeLinearMap a b -> SomeLinearMap (DualOf b) (DualOf a)
lflip (SomeLinearMap f) = SomeLinearMap (Flip f)

newtype Flip f = Flip f

instance (LinearMap f a b, a ~ DualOf da, b ~ DualOf db, da ~ DualOf a, db ~ DualOf b) => LinearMap (Flip f) db da where
  lmapFwd (Flip f) x = x |*> f
  lmapBack (Flip f) x = f <*| x

data ComposeLinearMap (a :: Type) (c :: Type) where
  ComposeLinearMap :: forall f g a b c. (LinearMap f b c, LinearMap g a b) => Proxy b -> f -> g -> ComposeLinearMap a c

instance () => LinearMap (ComposeLinearMap a c) a c where
  lmapFwd (ComposeLinearMap _ f g) x = f <*| (g <*| x)
  lmapBack = go
   where
    go (ComposeLinearMap b f g) = chainLMapBack b f g
    chainLMapBack :: forall f g b. (LinearMap f b c, LinearMap g a b) => Proxy b -> f -> g -> DualOf c -> DualOf a
    chainLMapBack _ f g dx = lmapBack @g @a @b g (lmapBack @f @b @c f dx)

instance Category SomeLinearMap where
  id = SomeLinearMap IdFun
  (.) :: forall a b c. SomeLinearMap b c -> SomeLinearMap a b -> SomeLinearMap a c
  SomeLinearMap f . SomeLinearMap g = SomeLinearMap (ComposeLinearMap (Proxy @b) f g)

data NegateFun = NegateFun

instance (Subtractive a, Subtractive (DualOf a), a ~ DualOf (DualOf a)) => LinearMap NegateFun a a where
  lmapFwd NegateFun = negate
  lmapBack NegateFun = negate

newtype ScalarMulFun a = ScalarMulFun a

instance
  ( MultiplicativeAction v
  , MultiplicativeAction (DualOf v)
  , Scalar (DualOf v) ~ Scalar v
  , DualOf (DualOf v) ~ v
  , a ~ Scalar v
  ) =>
  LinearMap (ScalarMulFun a) v v
  where
  lmapFwd (ScalarMulFun a) v = a *| v
  lmapBack (ScalarMulFun a) dv = a *| dv

data Fst u v = Fst

instance (Additive (DualOf v)) => LinearMap (Fst u v) (u, v) u where
  lmapFwd Fst = fst
  lmapBack Fst = (,zero)

data Snd u v = Snd

instance (Additive (DualOf u)) => LinearMap (Snd u v) (u, v) v where
  lmapFwd Snd = snd
  lmapBack Snd = (zero,)

lfst :: forall a b. (Additive (DualOf b)) => SomeLinearMap (a, b) a
lfst = SomeLinearMap (Fst @a @b)

lsnd :: forall a b. (Additive (DualOf a)) => SomeLinearMap (a, b) b
lsnd = SomeLinearMap (Snd @a @b)

lid :: forall a. SomeLinearMap a a
lid = SomeLinearMap IdFun

lzero :: forall a b. (Additive b, Additive (DualOf a)) => SomeLinearMap a b
lzero = SomeLinearMap (Zero @a @b)
