{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Downhill.Grad
  ( Dual (..), MetricTensor(..),
    HasGrad (..), GradBuilder, HasFullGrad, HasGradAffine
  )
where

import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace)
import Downhill.Linear.Expr (FullVector, BasicVector (VecBuilder))
import Data.AffineSpace (AffineSpace(Diff))

import qualified Data.VectorSpace as VectorSpace


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

-- TODO: add (VectorSpace m) constraint
class Dual s (MtVector m) (MtCovector m) => MetricTensor s m where
  type MtVector m :: Type
  type MtCovector m :: Type
  evalMetric :: m -> MtCovector m -> MtVector m
  -- | @innerProduct x m y == evalGrad x (evalMetric m y)@
  -- | @innerProduct x m y == innerProduct y m x@
  innerProduct :: MtCovector m -> m -> MtCovector m -> s
  innerProduct x m y = evalGrad x (evalMetric m y)
  sqrNorm :: m -> MtCovector m -> s
  sqrNorm m x = innerProduct x m x

class
  ( Dual (MScalar p) (Tang p) (Grad p)
  , MetricTensor (MScalar p) (Metric p)
  , MtVector (Metric p) ~ Tang p
  , MtCovector (Metric p) ~ Grad p
  , BasicVector (Tang p)
  , BasicVector (Grad p)
  ) =>
  HasGrad p
  where
  type MScalar p :: Type
  type Tang p :: Type
  type Grad p :: Type
  type Metric p :: Type

type GradBuilder v = VecBuilder (Grad v)

type HasFullGrad p = (HasGrad p, FullVector (Grad p))

type HasGradAffine p =
  ( AffineSpace p,
    HasGrad p,
    HasGrad (Tang p),
    Tang p ~ Diff p,
    Grad (Tang p) ~ Grad p
  )

instance Dual Integer Integer Integer  where
  evalGrad = (*)

instance MetricTensor Integer Integer where
  type (MtVector Integer) = Integer
  type (MtCovector Integer) = Integer
  evalMetric m x = m*x
  
instance HasGrad Integer where
  type MScalar Integer = Integer
  type Tang Integer = Integer
  type Grad Integer = Integer
  type Metric Integer = Integer

instance (Dual s a da, Dual s b db) => Dual s (a, b) (da, db) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s a da, Dual s b db, Dual s c dc) => Dual s (a, b, c) (da, db, dc) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance (MetricTensor s ma, MetricTensor s mb) => MetricTensor s (ma, mb) where
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

instance (MetricTensor s ma, MetricTensor s mb, MetricTensor s mc) => MetricTensor s (ma, mb, mc) where
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

instance MetricTensor Float Float where
  type MtVector Float = Float
  type MtCovector Float = Float
  evalMetric m dv = m*dv

instance HasGrad Float where
  type MScalar Float = Float
  type Grad Float = Float
  type Tang Float = Float
  type Metric Float = Float

instance Dual Double Double Double where
  evalGrad = (*)

instance MetricTensor Double Double where
  type MtVector Double = Double
  type MtCovector Double = Double
  evalMetric m dv = m*dv

instance HasGrad Double where
  type MScalar Double = Double
  type Grad Double = Double
  type Tang Double = Double
  type Metric Double = Double
