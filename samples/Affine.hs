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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.AffineSpace (AffineSpace ((.+^), (.-.)))
import qualified Data.AffineSpace as AffineSpace
import Data.Foldable (traverse_)
import Data.VectorSpace (AdditiveGroup (negateV, (^+^)), VectorSpace ((*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.DVar (BVar (BVar, bvarValue), backprop, constant, var)
import Downhill.Grad (Dual (evalGrad), HasGrad (Diff, Grad, Scalar), HasGradAffine)
import Downhill.Linear.BackGrad (BackGrad (BackGrad), realNode)
import Downhill.Linear.Expr (BackFun (BackFun), BasicVector (VecBuilder), DenseBuilder (DenseBuilder), DenseVector (DenseVector), Expr (ExprSum), FullVector (identityBuilder), toDenseBuilder)
import Downhill.Linear.Lift (lift1, lift1_dense)
import GHC.Generics (Generic)

data Point = Point Double Double
  deriving (Generic, Show)

data Vector = Vector Double Double
  deriving (Generic, Show)

data Gradient = Gradient Double Double
  deriving (Generic, Show)
  deriving (BasicVector, FullVector) via (DenseVector Gradient)

instance AdditiveGroup Vector

instance VectorSpace Vector

instance AdditiveGroup Gradient

instance VectorSpace Gradient

instance AffineSpace Point where
  type Diff Point = Vector
  Point x y .+^ Vector dx dy = Point (x .+^ dx) (y .+^ dy)
  Point x1 y1 .-. Point x2 y2 = Vector (x1 .-. x2) (y1 .-. y2)

instance Dual Double Gradient Vector where
  evalGrad (Gradient dx dy) (Vector x y) = dx * x + dy * y

instance HasGrad Vector where
  type Scalar Vector = Double
  type Diff Vector = Vector
  type Grad Vector = Gradient

instance HasGrad Point where
  type Scalar Point = Double
  type Diff Point = Vector
  type Grad Point = Gradient

constPoint :: Point -> BVar r Point
constPoint = constant

varPoint :: Point -> BVar Gradient Point
varPoint = var

sqrNorm :: BVar r Vector -> BVar r Double
sqrNorm (BVar (Vector x y) dv) = BVar normValue (lift1_dense bp dv)
  where
    normValue = x ** 2 + y ** 2
    bp :: Double -> Gradient -- same as `Grad Double -> Grad Vector`
    bp a = (2 * a) *^ Gradient x y

distance :: BVar r Point -> BVar r Point -> BVar r Double
distance x y = sqrt $ sqrNorm (x .-. y)

class MetricTensor g dv v where
  evalMetric :: g -> dv -> v

class HilbertSpace dv v where
  riesz :: dv -> v

--coriesz :: v -> dv

instance HilbertSpace Gradient Vector where
  riesz (Gradient x y) = Vector x y

--coriesz (Vector x y) = Gradient x y

data L2 = L2

instance (HilbertSpace dv v, Dual s dv v) => MetricTensor L2 dv v where
  evalMetric L2 = riesz

type HilbertManifold p = (HasGrad p, Scalar p ~ Double, MetricTensor L2 (Grad p) (Diff p))

updateStep :: forall g p. (HasGradAffine p, MetricTensor g (Grad p) (Diff p)) => g -> Scalar p -> Grad p -> p -> p
updateStep metric lr grad x = x .+^ step
  where
    dir :: Diff p
    dir = evalMetric metric grad
    step = lr *^ dir

data Triangle = Triangle Point Point Point

totalDistance :: forall r. Triangle -> BVar r Point -> BVar r Double
totalDistance (Triangle p1 p2 p3) x = d1 + d2 + d3
  where
    d1, d2, d3 :: BVar r Double
    d1 = distance x (constant p1)
    d2 = distance x (constant p2)
    d3 = distance x (constant p3)

data Iterate p z = Iterate
  { itSolution :: p,
    itDistance :: z,
    itGradNorm :: Scalar p
  }

deriving instance (Show p, Show z, Show (Scalar p)) => Show (Iterate p z)

affineStep ::
  forall g p z.
  ( HasGradAffine p,
    HasGrad z,
    MetricTensor g (Grad p) (Diff p)
  ) =>
  (p -> BVar (Grad p) z) ->
  Grad z ->
  Scalar p ->
  g ->
  p ->
  Iterate p z
affineStep objectiveFunc one stepSize metric = nextIter
  where
    nextIter :: p -> Iterate p z
    nextIter x = Iterate (x .+^ (stepSize *^ step)) (bvarValue dist) (evalGrad grad step)
      where
        dist :: BVar (Grad p) z
        dist = objectiveFunc x
        grad :: Grad p
        grad = backprop dist one
        step :: Diff p
        step = evalMetric metric grad

affineIterate ::
  forall g p z.
  ( HasGradAffine p,
    MetricTensor g (Grad p) (Diff p),
    HasGrad z
  ) =>
  (p -> BVar (Grad p) z) ->
  Grad z ->
  Scalar p ->
  g ->
  p ->
  [Iterate p z]
affineIterate objectiveFunc one stepSize metric x0 = iterate (step . itSolution) (step x0)
  where
    step = affineStep objectiveFunc one stepSize metric

solveFermatPoint :: Triangle -> L2 -> Point -> [Iterate Point Double]
solveFermatPoint triangle = affineIterate distF 1 0.1
  where
    distF :: Point -> BVar Gradient Double
    distF x = totalDistance triangle (var x)

main :: IO ()
main = traverse_ print iters
  where
    triangle = Triangle (Point 0 0) (Point 0 1) (Point 1 0)
    x0 = Point 1 1
    lr = L2
    iters :: [Iterate Point Double]
    iters = take 20 $ solveFermatPoint triangle lr x0
