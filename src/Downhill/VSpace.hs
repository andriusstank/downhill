{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.VSpace (
  VSpace (..),
  (*^),
  (^*),
  InnerSpace (..),
  CScalar (..),
  RealScalar,
  InnerSpaceMetric(..)
)
where

import Downhill.Linear.Fun (DualOf, AsLinearMap (toLinearMap), Metric, LinearMap (..))
import Downhill.Orphans ()
import NumHask.Prelude

class
  ( Subtractive v
  , Subtractive (DualOf v)
  , Scalar (Scalar v) ~ Scalar v
  , Scalar v ~ Scalar (DualOf v)
  , Scalar v ~ DualOf (Scalar v)
  , FromInteger (Scalar v) -- TODO: maybe FromInteger should be a superclass of Multiplicative (and Ring)
  , Ring (Scalar v)
  , Multiplicative (Scalar v)
  , MultiplicativeAction v
  , MultiplicativeAction (DualOf v)
  , v ~ DualOf (DualOf v)
  ) =>
  VSpace v
  where
  infix 7 |*|
  (|*|) :: DualOf v -> v -> Scalar v
  default (|*|) :: (GDual (Scalar v) (Rep v) (Rep (DualOf v)), Generic (DualOf v), Generic v) => DualOf v -> v -> Scalar v
  dv |*| v = gevalGrad (from dv) (from v)

instance (Scalar a ~ Scalar b, VSpace a, VSpace b) => VSpace (a, b) where
  (a, b) |*| (x, y) = a |*| x + b |*| y

instance (Scalar a ~ Scalar b, Scalar a ~ Scalar c, VSpace a, VSpace b, VSpace c) => VSpace (a, b, c) where
  (a, b, c) |*| (x, y, z) = a |*| x + b |*| y + c |*| z

(*^) :: (VSpace v) => Scalar v -> v -> v
(*^) = (*|)

(^*) :: (VSpace v) => v -> Scalar v -> v
(^*) = (|*)

class (VSpace v) => InnerSpace v where
  riesz :: v -> DualOf v
  coriesz :: DualOf v -> v
  (<.>) :: v -> v -> Scalar v
  sqrNorm :: (InnerSpace v) => v -> Scalar v
  sqrNorm x = x <.> x

class GDual s v dv where
  gevalGrad :: dv p -> v p -> s

instance (s ~ Scalar v, VSpace v, dv ~ DualOf v) => GDual s (K1 x v) (K1 x dv) where
  gevalGrad (K1 dv) (K1 v) = dv |*| v

instance (GDual s v dv) => GDual s (M1 x y v) (M1 x' y' dv) where
  gevalGrad (M1 dv) (M1 v) = gevalGrad dv v

instance (Additive s, GDual s u du, GDual s v dv) => GDual s (u :*: v) (du :*: dv) where
  gevalGrad (du :*: dv) (u :*: v) = gevalGrad du u + gevalGrad dv v

instance GDual s V1 V1 where
  gevalGrad = \case {}

instance (Additive s) => GDual s U1 U1 where
  gevalGrad U1 = zero

instance InnerSpace Float where
  riesz = id
  coriesz = id
  (<.>) = (*)
instance InnerSpace Double where
  riesz = id
  coriesz = id
  (<.>) = (*)
instance InnerSpace Int where
  riesz = id
  coriesz = id
  (<.>) = (*)
instance InnerSpace Integer where
  riesz = id
  coriesz = id
  (<.>) = (*)

instance (Ring a, FromInteger a) => VSpace (Complex a) where
  (|*|) = (*)

instance VSpace (Ratio Integer) where
  (|*|) = (*)

instance (RealScalar a) => InnerSpace (Complex a) where
  riesz = conj
  coriesz = conj
  x <.> y = x * conj y

instance InnerSpace (Ratio Integer) where
  riesz = id
  coriesz = id
  (<.>) = (*)

instance VSpace Float where
  (|*|) = (*)
instance VSpace Double where
  (|*|) = (*)
instance VSpace Int where
  (|*|) = (*)
instance VSpace Integer where
  (|*|) = (*)

instance
  ( Scalar (Generically v) ~ Scalar (Generically (DualOf v))
  , Scalar (Generically v) ~ DualOf (Scalar (Generically v))
  , Scalar (Generically v) ~ Scalar (Scalar (Generically v))
  , Multiplicative (Scalar (Generically v))
  , FromInteger (Scalar (Generically v))
  , DualOf (DualOf v) ~ v
  , Generic v
  , Generic (DualOf v)
  , Subtractive (Generically (DualOf v))
  , Subtractive (Generically v)
  , Subtractive (Scalar (Generically v))
  , MultiplicativeAction (Generically v)
  , MultiplicativeAction (Generically (DualOf v))
  , GVSpace (Rep (DualOf v)) (Rep v) (Scalar (Generically v))
  ) =>
  VSpace (Generically v)
  where
  Generically dx |*| Generically x = gdvvmul (from dx) (from x)

class GVSpace df f a where
  gdvvmul :: df p -> f p -> a

instance (VSpace v, dv ~ DualOf v, Scalar dv ~ a, Scalar v ~ a) => GVSpace (K1 i dv) (K1 i v) a where
  gdvvmul (K1 x) (K1 y) = x |*| y

instance (GVSpace df f a) => GVSpace (M1 i1 m1 df) (M1 i2 m2 f) a where
  gdvvmul (M1 x) (M1 y) = gdvvmul x y

instance (GVSpace df f a, GVSpace dg g a, Additive a) => GVSpace (df :*: dg) (f :*: g) a where
  gdvvmul (x1 :*: x2) (y1 :*: y2) = gdvvmul x1 y1 + gdvvmul x2 y2

instance (Additive a) => GVSpace U1 U1 a where
  gdvvmul _ _ = zero

class
  ( Ring a
  , DualOf a ~ a
  , Scalar a ~ a
  , MultiplicativeAction a
  , InnerSpace a
  , RealType (RealType a) ~ RealType a
  , FromInteger a
  ) =>
  CScalar a
  where
  type RealType a :: Type
  type RealType a = a
  conj :: a -> a
  default conj :: (RealScalar a) => a -> a
  conj = id

  -- | sqnorm x = x * conj x
  sqnorm :: a -> RealType a
  default sqnorm :: (RealScalar a) => a -> RealType a
  sqnorm x = x * x

class (RealType a ~ a, CScalar a) => RealScalar a
instance (RealType a ~ a, CScalar a) => RealScalar a

instance CScalar Float
instance CScalar Double
instance CScalar Int
instance CScalar Integer

instance (RealScalar a) => CScalar (Complex a) where
  type RealType (Complex a) = a
  conj (Complex (x, y)) = Complex (x, negate y)
  sqnorm (Complex (x, y)) = sqnorm x + sqnorm y

instance CScalar (Ratio Integer) where
  type RealType (Ratio Integer) = Ratio Integer

data InnerSpaceMetric = InnerSpaceMetric

instance (v ~ DualOf dv, dv ~ DualOf v, InnerSpace v) => AsLinearMap InnerSpaceMetric v dv where
    toLinearMap InnerSpaceMetric = LinearMap riesz riesz

instance (InnerSpace v) => Metric InnerSpaceMetric v
