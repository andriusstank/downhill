{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Grad
  ( Dual (..),
    MetricTensor (..),
    HasGrad (..),
    GradBuilder,
    HasFullGrad,
    HasGradAffine,
  )
where

import Data.AffineSpace (AffineSpace (Diff))
import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace (Scalar, (*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.Linear.Expr (BasicVector (VecBuilder), FullVector)
import GHC.Generics (Generic)

-- | Dual of a vector @v@ is a linear map @v -> Scalar v@.
class
  ( AdditiveGroup s,
    VectorSpace v,
    VectorSpace dv,
    VectorSpace.Scalar v ~ s,
    VectorSpace.Scalar dv ~ s
  ) =>
  Dual s v dv
  where
  -- if evalGrad goes to HasGrad class, parameter p is ambiguous
  evalGrad :: dv -> v -> s

-- | @MetricTensor@ converts gradients to vectors.
--
-- It is really inverse of a metric tensor, because it maps cotangent
-- space into tangent space. Gradient descent doesn't need metric tensor,
-- it needs inverse.

class
  ( Dual (Scalar g) (MtVector g) (MtCovector g),
    VectorSpace g
  ) =>
  MetricTensor g
  where
  type MtVector g :: Type
  type MtCovector g :: Type

  -- | @m@ must be symmetric:
  --
  -- @evalGrad x (evalMetric m y) = evalGrad y (evalMetric m x)@
  evalMetric :: g -> MtCovector g -> MtVector g

  -- | @innerProduct m x y = evalGrad x (evalMetric m y)@
  innerProduct :: g -> MtCovector g -> MtCovector g -> Scalar g
  innerProduct g x y = evalGrad x (evalMetric g y)

  -- | @sqrNorm m x = innerProduct m x x@
  sqrNorm :: g -> MtCovector g -> Scalar g
  sqrNorm g x = innerProduct g x x

-- | @HasGrad@ is a collection of types and constraints that are useful
-- in many places. It helps to keep type signatures short.

-- TODO: FullVector or not?
-- TODO: Metric or not?
class
  ( Dual (MScalar p) (Tang p) (Grad p),
    MetricTensor (Metric p),
    MtVector (Metric p) ~ Tang p,
    MtCovector (Metric p) ~ Grad p,
    BasicVector (Tang p),
    BasicVector (Grad p)
  ) =>
  HasGrad p
  where
  -- | Scalar of @Tang p@ and @Grad p@.
  type MScalar p :: Type

  -- | Tangent vector of manifold @p@. If p is 'AffineSpace', @Tang p@ should
  -- be @'Diff' p@. If @p@ is 'VectorSpace', @Tang p@ might be the same as @p@ itself.
  type Tang p :: Type

  -- | Dual of tangent space of @p@.
  type Grad p :: Type

  -- | A 'MetricTensor'.
  type Metric p :: Type

type GradBuilder v = VecBuilder (Grad v)

type HasFullGrad p = (HasGrad p, FullVector (Grad p))

type HasGradAffine p =
  ( AffineSpace p,
    HasGrad p,
    HasGrad (Tang p),
    Tang p ~ Diff p,
    Tang (Tang p) ~ Tang p,
    Grad (Tang p) ~ Grad p
  )

instance Dual Integer Integer Integer where
  evalGrad = (*)

instance MetricTensor Integer where
  type MtVector Integer = Integer
  type MtCovector Integer = Integer
  evalMetric m x = m * x

instance HasGrad Integer where
  type MScalar Integer = Integer
  type Tang Integer = Integer
  type Grad Integer = Integer
  type Metric Integer = Integer

instance (Dual s a da, Dual s b db) => Dual s (a, b) (da, db) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s a da, Dual s b db, Dual s c dc) => Dual s (a, b, c) (da, db, dc) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance (MetricTensor ma, MetricTensor mb, Scalar ma ~ Scalar mb) => MetricTensor (ma, mb) where
  type MtVector (ma, mb) = (MtVector ma, MtVector mb)
  type MtCovector (ma, mb) = (MtCovector ma, MtCovector mb)
  evalMetric (ma, mb) (a, b) = (evalMetric ma a, evalMetric mb b)
  sqrNorm (ma, mb) (a, b) = sqrNorm ma a ^+^ sqrNorm mb b

instance
  ( HasGrad a,
    HasGrad b,
    MScalar b ~ MScalar a
  ) =>
  HasGrad (a, b)
  where
  type MScalar (a, b) = MScalar a
  type Grad (a, b) = (Grad a, Grad b)
  type Tang (a, b) = (Tang a, Tang b)
  type Metric (a, b) = (Metric a, Metric b)

instance
  ( MetricTensor ma,
    MetricTensor mb,
    MetricTensor mc,
    Scalar ma ~ Scalar mb,
    Scalar ma ~ Scalar mc
  ) =>
  MetricTensor (ma, mb, mc)
  where
  type MtVector (ma, mb, mc) = (MtVector ma, MtVector mb, MtVector mc)
  type MtCovector (ma, mb, mc) = (MtCovector ma, MtCovector mb, MtCovector mc)
  evalMetric (ma, mb, mc) (a, b, c) = (evalMetric ma a, evalMetric mb b, evalMetric mc c)
  sqrNorm (ma, mb, mc) (a, b, c) = sqrNorm ma a ^+^ sqrNorm mb b ^+^ sqrNorm mc c

instance
  ( HasGrad a,
    HasGrad b,
    HasGrad c,
    MScalar b ~ MScalar a,
    MScalar c ~ MScalar a
  ) =>
  HasGrad (a, b, c)
  where
  type MScalar (a, b, c) = MScalar a
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Tang (a, b, c) = (Tang a, Tang b, Tang c)
  type Metric (a, b, c) = (Metric a, Metric b, Metric c)

instance Dual Float Float Float where
  evalGrad = (*)

instance MetricTensor Float where
  type MtVector Float = Float
  type MtCovector Float = Float
  evalMetric m dv = m * dv

instance HasGrad Float where
  type MScalar Float = Float
  type Grad Float = Float
  type Tang Float = Float
  type Metric Float = Float

instance Dual Double Double Double where
  evalGrad = (*)

instance MetricTensor Double where
  type MtVector Double = Double
  type MtCovector Double = Double
  evalMetric m dv = m * dv

instance HasGrad Double where
  type MScalar Double = Double
  type Grad Double = Double
  type Tang Double = Double
  type Metric Double = Double

newtype L2 v = L2 (Scalar v)
  deriving (Generic)

instance AdditiveGroup (Scalar v) => AdditiveGroup (L2 v)

instance (AdditiveGroup (Scalar v), Num (Scalar v)) => VectorSpace (L2 v) where
  type Scalar (L2 v) = Scalar v
  x *^ L2 y = L2 (x * y)

instance (AdditiveGroup a, Num a, a ~ Scalar v, Dual a v v) => MetricTensor (L2 v) where
  type MtVector (L2 v) = v
  type MtCovector (L2 v) = v
  evalMetric (L2 a) u = a *^ u
  innerProduct (L2 a) x y = a * evalGrad x y
  sqrNorm g x = innerProduct g x x
