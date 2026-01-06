{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.VSpace (
  VSpace (..),
  (*^),
  (^*),
  InnerSpace (..),
  sqrNorm,
)
where

import Downhill.Linear.Fun (DualOf)
import Downhill.Orphans ()
import NumHask.Prelude

class
  ( Subtractive v
  , Subtractive (DualOf v)
  , Scalar (Scalar v) ~ Scalar v
  , Scalar v ~ Scalar (DualOf v)
  , Scalar v ~ DualOf (Scalar v)
  , Ring (Scalar v)
  , Multiplicative (Scalar v)
  , MultiplicativeAction v
  , MultiplicativeAction (DualOf v)
  , v ~ DualOf (DualOf v)
  ) =>
  VSpace v
  where
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

instance (GDual s v dv) => GDual s (M1 x y v) (M1 x y' dv) where
  gevalGrad (M1 dv) (M1 v) = gevalGrad dv v

instance (Additive s, GDual s u du, GDual s v dv) => GDual s (u :*: v) (du :*: dv) where
  gevalGrad (du :*: dv) (u :*: v) = gevalGrad du u + gevalGrad dv v

instance GDual s V1 V1 where
  gevalGrad = \case {}

instance (Additive s) => GDual s U1 U1 where
  gevalGrad U1 = zero

instance VSpace Float where
  (|*|) = (*)
instance VSpace Double where
  (|*|) = (*)
instance VSpace Int where
  (|*|) = (*)
instance VSpace Integer where
  (|*|) = (*)
