{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Grad
  ( Dual (..),
    HasGrad (..), MScalar,
    GradBuilder,
    HasGradAffine,
  )
where

import Data.AffineSpace (AffineSpace (Diff))
import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^), zeroV), VectorSpace(Scalar))
import Downhill.Linear.Expr (BasicVector (VecBuilder))
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), U1 (U1), V1, (:*:) ((:*:)))

-- | Dual of a vector @v@ is a linear map @v -> Scalar v@.
class
  ( 
    Scalar v ~ Scalar dv,
    AdditiveGroup (Scalar v),
    VectorSpace v,
    VectorSpace dv
  ) =>
  Dual v dv
  where
  -- if evalGrad goes to HasGrad class, parameter p is ambiguous
  evalGrad :: dv -> v -> Scalar v
  default evalGrad :: (GDual (Scalar v) (Rep v) (Rep dv), Generic dv, Generic v) => dv -> v -> Scalar v
  evalGrad dv v = gevalGrad (from dv) (from v)

type MScalar p = Scalar (Tang p)

-- | @HasGrad@ is a collection of types and constraints that are useful
-- in many places. It helps to keep type signatures short.
class
  ( Dual (Tang p) (Grad p),
    BasicVector (Tang p),
    BasicVector (Grad p),
    Scalar (Tang p) ~ MScalar p,
    Scalar (Grad p) ~ MScalar p
  ) =>
  HasGrad p
  where
  -- | Scalar of @Tang p@ and @Grad p@.
  --type MScalar p :: Type

  -- | Tangent vector of manifold @p@. If p is 'AffineSpace', @Tang p@ should
  -- be @'Diff' p@. If @p@ is 'VectorSpace', @Tang p@ might be the same as @p@ itself.
  type Tang p :: Type

  -- | Dual of tangent space of @p@.
  type Grad p :: Type

type GradBuilder v = VecBuilder (Grad v)

type HasGradAffine p =
  ( AffineSpace p,
    HasGrad p,
    HasGrad (Tang p),
    Tang p ~ Diff p,
    Tang (Tang p) ~ Tang p,
    Grad (Tang p) ~ Grad p
  )

instance Dual Integer Integer where
  evalGrad = (*)

instance HasGrad Integer where
  type Tang Integer = Integer
  type Grad Integer = Integer

instance (Scalar a ~ Scalar b, Dual a da, Dual b db) => Dual (a, b) (da, db) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Scalar a ~ Scalar b, Scalar a ~ Scalar c, Dual a da, Dual b db, Dual c dc) => Dual (a, b, c) (da, db, dc) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance
  ( HasGrad a,
    HasGrad b,
    MScalar b ~ MScalar a
  ) =>
  HasGrad (a, b)
  where
  type Grad (a, b) = (Grad a, Grad b)
  type Tang (a, b) = (Tang a, Tang b)

instance
  ( HasGrad a,
    HasGrad b,
    HasGrad c,
    MScalar b ~ MScalar a,
    MScalar c ~ MScalar a
  ) =>
  HasGrad (a, b, c)
  where
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Tang (a, b, c) = (Tang a, Tang b, Tang c)

instance Dual Float Float where
  evalGrad = (*)

instance HasGrad Float where
  type Grad Float = Float
  type Tang Float = Float

instance Dual Double Double where
  evalGrad = (*)

instance HasGrad Double where
  type Grad Double = Double
  type Tang Double = Double

class GDual s v dv where
  gevalGrad :: dv p -> v p -> s

instance (s ~ Scalar v, Dual v dv) => GDual s (K1 x v) (K1 x dv) where
  gevalGrad (K1 dv) (K1 v) = evalGrad dv v

instance (GDual s v dv) => GDual s (M1 x y v) (M1 x y' dv) where
  gevalGrad (M1 dv) (M1 v) = gevalGrad dv v

instance (AdditiveGroup s, GDual s u du, GDual s v dv) => GDual s (u :*: v) (du :*: dv) where
  gevalGrad (du :*: dv) (u :*: v) = gevalGrad du u ^+^ gevalGrad dv v

instance GDual s V1 V1 where
  gevalGrad = \case {}

instance AdditiveGroup s => GDual s U1 U1 where
  gevalGrad U1 = zeroV
