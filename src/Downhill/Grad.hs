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

class Dual s (MTVector m) (MTCovector m) => MetricTensor s m where
  type MTVector m :: Type
  type MTCovector m :: Type
  evalMetric :: m -> MTCovector m -> MTVector m
  -- | @innerProduct x m y == evalGrad x (evalMetric m y)@
  -- | @innerProduct x m y == innerProduct y m x@
  innerProduct :: MTCovector m -> m -> MTCovector m -> s
  innerProduct x m y = evalGrad x (evalMetric m y)
  sqrNorm :: m -> MTCovector m -> s
  sqrNorm m x = innerProduct x m x

class
  ( Dual (Scalar p) (Tang p) (Grad p)
  , MetricTensor (Scalar p) (Metric p)
  , MTVector (Metric p) ~ Tang p
  , MTCovector (Metric p) ~ Grad p
  , BasicVector (Tang p)
  , BasicVector (Grad p)
  ) =>
  HasGrad p
  where
  type Scalar p :: Type
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
  type (MTVector Integer) = Integer
  type (MTCovector Integer) = Integer
  evalMetric m x = m*x
  
instance HasGrad Integer where
  type Scalar Integer = Integer
  type Tang Integer = Integer
  type Grad Integer = Integer
  type Metric Integer = Integer

instance (Dual s a da, Dual s b db) => Dual s (a, b) (da, db) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s a da, Dual s b db, Dual s c dc) => Dual s (a, b, c) (da, db, dc) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance (MetricTensor s ma, MetricTensor s mb) => MetricTensor s (ma, mb) where
  type MTVector (ma, mb) = (MTVector ma, MTVector mb)
  type MTCovector (ma, mb) = (MTCovector ma, MTCovector mb)
  evalMetric (ma, mb) (a, b) = (evalMetric ma a, evalMetric mb b)
  sqrNorm (ma, mb) (a, b) = sqrNorm ma a ^+^ sqrNorm mb b

instance
  ( HasGrad a,
    HasGrad b,
    Scalar b ~ Scalar a
  ) =>
  HasGrad (a, b)
  where
  type Scalar (a, b) = Scalar a
  type Grad (a, b) = (Grad a, Grad b)
  type Tang (a, b) = (Tang a, Tang b)
  type Metric (a, b) = (Metric a, Metric b)

instance (MetricTensor s ma, MetricTensor s mb, MetricTensor s mc) => MetricTensor s (ma, mb, mc) where
  type MTVector (ma, mb, mc) = (MTVector ma, MTVector mb, MTVector mc)
  type MTCovector (ma, mb, mc) = (MTCovector ma, MTCovector mb, MTCovector mc)
  evalMetric (ma, mb, mc) (a, b, c) = (evalMetric ma a, evalMetric mb b, evalMetric mc c)
  sqrNorm (ma, mb, mc) (a, b, c) = sqrNorm ma a ^+^ sqrNorm mb b ^+^ sqrNorm mc c

instance
  ( HasGrad a,
    HasGrad b,
    HasGrad c,
    Scalar b ~ Scalar a,
    Scalar c ~ Scalar a
  ) =>
  HasGrad (a, b, c)
  where
  type Scalar (a, b, c) = Scalar a
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Tang (a, b, c) = (Tang a, Tang b, Tang c)
  type Metric (a, b, c) = (Metric a, Metric b, Metric c)

instance Dual Float Float Float where
  evalGrad = (*)

instance MetricTensor Float Float where
  type MTVector Float = Float
  type MTCovector Float = Float
  evalMetric m dv = m*dv

instance HasGrad Float where
  type Scalar Float = Float
  type Grad Float = Float
  type Tang Float = Float
  type Metric Float = Float

instance Dual Double Double Double where
  evalGrad = (*)

instance MetricTensor Double Double where
  type MTVector Double = Double
  type MTCovector Double = Double
  evalMetric m dv = m*dv

instance HasGrad Double where
  type Scalar Double = Double
  type Grad Double = Double
  type Tang Double = Double
  type Metric Double = Double
