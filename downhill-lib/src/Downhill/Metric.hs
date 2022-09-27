{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Downhill.Metric
  ( MetricTensor (..)
  )
where

import Data.VectorSpace ((^+^))
import Downhill.Grad (Dual (evalGrad), HasGrad (Grad, Tang), MScalar)

-- | @MetricTensor@ converts gradients to vectors.
--
-- It is really inverse of a metric tensor, because it maps cotangent
-- space into tangent space. Gradient descent doesn't need metric tensor,
-- it needs inverse.
class Dual (Tang p) (Grad p) => MetricTensor p g where
  -- | @m@ must be symmetric:
  --
  -- @evalGrad x (evalMetric m y) = evalGrad y (evalMetric m x)@
  evalMetric :: g -> Grad p -> Tang p

  -- | @innerProduct m x y = evalGrad x (evalMetric m y)@
  innerProduct :: g -> Grad p -> Grad p -> MScalar p
  innerProduct g x y = evalGrad @(Tang p) @(Grad p) x (evalMetric @p g y)

  -- | @sqrNorm m x = innerProduct m x x@
  sqrNorm :: g -> Grad p -> MScalar p
  sqrNorm g x = innerProduct @p g x x

instance MetricTensor Integer Integer where
  evalMetric m x = m * x

instance (MScalar a ~ MScalar b, MetricTensor a ma, MetricTensor b mb) => MetricTensor (a, b) (ma, mb) where
  evalMetric (ma, mb) (a, b) = (evalMetric @a ma a, evalMetric @b mb b)
  sqrNorm (ma, mb) (a, b) = sqrNorm @a ma a ^+^ sqrNorm @b mb b

instance
  ( MScalar a ~ MScalar b,
    MScalar a ~ MScalar c,
    MetricTensor a ma,
    MetricTensor b mb,
    MetricTensor c mc
  ) =>
  MetricTensor (a, b, c) (ma, mb, mc)
  where
  evalMetric (ma, mb, mc) (a, b, c) = (evalMetric @a ma a, evalMetric @b mb b, evalMetric @c mc c)
  sqrNorm (ma, mb, mc) (a, b, c) = sqrNorm @a ma a ^+^ sqrNorm @b mb b ^+^ sqrNorm @c mc c

instance MetricTensor Float Float where
  evalMetric m dv = m * dv

instance MetricTensor Double Double where
  evalMetric m dv = m * dv

data L2 = L2

instance (Dual (Tang p) (Grad p), Grad p ~ Tang p) => MetricTensor p L2 where
  evalMetric L2 v = v
