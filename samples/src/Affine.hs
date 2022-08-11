{-
Points don't make a vector space. We can add vectors, add vector to a
point, subtract points, but adding two points make no sense. Yet we can
differentiate functions defined over points.

This sample uses gradient descent to find Fermat point of a triangle ABC.
It's a point X that minizes the sum of distances from X to each triangle
vertex.

-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.AffineSpace (AffineSpace ((.+^), (.-.)), (.-^))
import qualified Data.AffineSpace as AffineSpace
import Data.Foldable (traverse_)
import Data.VectorSpace (AdditiveGroup, VectorSpace ((*^)))
import Downhill.BVar (BVar (BVar, bvarValue), backprop, constant, var)
import Downhill.Grad
  ( Dual (evalGrad),
    Manifold (Grad, Tang),
    HasGradAffine, MScalar
  )
import Downhill.Linear.Expr (BasicVector, DenseVector (DenseVector))
import Downhill.Linear.Lift (lift1_dense)
import GHC.Generics (Generic)
import Downhill.Metric (MetricTensor (evalMetric))

data Point = Point Double Double
  deriving (Generic, Show)

data Vector = Vector Double Double
  deriving (Generic, Show)
  deriving (BasicVector) via (DenseVector Vector)

data Gradient = Gradient Double Double
  deriving (Generic, Show)
  deriving (BasicVector) via (DenseVector Gradient)

instance AdditiveGroup Vector

instance VectorSpace Vector

instance AdditiveGroup Gradient

instance VectorSpace Gradient

instance AffineSpace Point where
  type Diff Point = Vector
  Point x y .+^ Vector dx dy = Point (x .+^ dx) (y .+^ dy)
  Point x1 y1 .-. Point x2 y2 = Vector (x1 .-. x2) (y1 .-. y2)

instance Dual Vector Gradient where
  evalGrad (Gradient dx dy) (Vector x y) = dx * x + dy * y

instance Manifold Vector where
  type Tang Vector = Vector
  type Grad Vector = Gradient

instance Manifold Point where
  type Tang Point = Vector
  type Grad Point = Gradient

sqrNormBp :: BVar r Vector -> BVar r Double
sqrNormBp (BVar (Vector x y) dv) = BVar normValue (lift1_dense bp dv)
  where
    normValue = x ** 2 + y ** 2
    bp :: Double -> Gradient -- same as `Grad Double -> Grad Vector`
    bp a = (2 * a) *^ Gradient x y

distance :: BVar r Point -> BVar r Point -> BVar r Double
distance x y = sqrt $ sqrNormBp (x .-. y)

-- L2 norm
data L2 = L2
  deriving (Generic)

instance MetricTensor Point L2 where
  evalMetric L2 (Gradient x y) = Vector x y

data Triangle = Triangle Point Point Point

-- Objective function  to minimize
totalDistance :: forall r. Triangle -> BVar r Point -> BVar r Double
totalDistance (Triangle p1 p2 p3) x = d1 + d2 + d3
  where
    d1, d2, d3 :: BVar r Double
    d1 = distance x (constant p1)
    d2 = distance x (constant p2)
    d3 = distance x (constant p3)

data Iterate p z = Iterate
  { itPoint :: p,
    itValue :: z, -- value of objective at itPoint
    itGradNorm :: MScalar p -- gradient of objective function at itPoint, hopefully converging to zero
  }

deriving instance (Show p, Show z, Show (MScalar p)) => Show (Iterate p z)

type PlainScalar z =
  ( Tang z ~ z,
    Grad z ~ z,
    BasicVector z,
    Num z,
    Manifold z
  )

affineStep ::
  forall p g z.
  ( HasGradAffine p,
    MetricTensor p g,
    PlainScalar z
  ) =>
  (p -> BVar (Grad p) z) ->
  MScalar p ->
  g ->
  p ->
  Iterate p z
affineStep objectiveFunc stepSize metric = nextIter
  where
    nextIter :: p -> Iterate p z
    nextIter x = Iterate (x .-^ (stepSize *^ step)) (bvarValue dist) (evalGrad grad step)
      where
        dist :: BVar (Grad p) z
        dist = objectiveFunc x
        grad :: Grad p
        grad = backprop dist 1
        step :: Tang p
        step = evalMetric @p metric grad

affineIterate ::
  forall p g z.
  ( HasGradAffine p,
    MetricTensor p g,
    PlainScalar z
  ) =>
  (p -> BVar (Grad p) z) ->
  MScalar p ->
  g ->
  p ->
  [Iterate p z]
affineIterate objectiveFunc stepSize metric x0 = iterate (step . itPoint) (step x0)
  where
    step = affineStep objectiveFunc stepSize metric

solveFermatPoint :: Triangle -> Double -> Point -> [Iterate Point Double]
solveFermatPoint triangle lr = affineIterate @Point @L2 @Double distF lr L2
  where
    distF :: Point -> BVar Gradient Double
    distF x = totalDistance triangle (var x)

main :: IO ()
main = traverse_ print iters
  where
    triangle = Triangle (Point 0 0) (Point 0 1) (Point 1 0)
    x0 = Point 1 1
    iters :: [Iterate Point Double]
    iters = take 20 $ solveFermatPoint triangle 0.1 x0
