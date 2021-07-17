{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.VectorSpace (AdditiveGroup, VectorSpace ((*^)))
import GHC.Generics (Generic)
import Data.AffineSpace (AffineSpace ((.+^), (.-.)))

import qualified Data.VectorSpace as VectorSpace
import qualified Data.AffineSpace as AffineSpace
import Downhill.Grad (HasGrad(Scalar, Diff, Grad), Dual (evalGrad))
import Downhill.Linear.Expr (BasicVector(VecBuilder), DenseVector (DenseVector), FullVector, DenseBuilder (DenseBuilder), toDenseBuilder, Expr (ExprSum), BackFun (BackFun))
import Downhill.DVar (BVar, constant, var, DVar (DVar))
import Downhill.Linear.Lift (lift1, LinFun1 (LinFun1), lift1'')
import Downhill.Linear.BackGrad (BackGrad(BackGrad), realNode)

data Point = Point Double Double
    deriving Generic

data Vector = Vector Double Double
    deriving Generic

data Gradient = Gradient Double Double
    deriving Generic
    deriving (BasicVector, FullVector) via (DenseVector Gradient)

instance AdditiveGroup Vector
instance VectorSpace Vector

instance AdditiveGroup Gradient
instance VectorSpace Gradient

instance AffineSpace Point where
    type Diff Point = Vector
    Point x y .+^ Vector dx dy = Point (x.+^dx) (y.+^dy)
    Point x1 y1 .-. Point x2 y2 = Vector (x1.-.x2) (y1.-.y2)

instance Dual Double Gradient Vector where
    evalGrad (Gradient dx dy) (Vector x y) = dx*x + dy*y

instance HasGrad Vector where
    type Scalar Vector = Double
    type Diff Vector = Vector
    type Grad Vector = Gradient

instance HasGrad Point where
    type Scalar Point = Double
    type Diff Point = Vector
    type Grad Point = Gradient

constPoint :: Point -> BVar a Point
constPoint = constant

varPoint :: Point -> BVar Gradient Point
varPoint = var

subPoint :: BVar a Point -> BVar a Point -> BVar a Vector
subPoint = (.-.)

sqrNorm :: BVar a Vector -> BVar a Double
sqrNorm (DVar (Vector x y) dv) = DVar normValue (lift1 back dv)
    where normValue = x**2 + y**2
          back :: LinFun1 Gradient Double
          back = LinFun1 @Double (toDenseBuilder . go)
          go :: Double -> Gradient
          go a = (2*a) *^ Gradient x y

sqrNorm' :: BVar a Vector -> BVar a Double
sqrNorm' (DVar (Vector x y) dv@(BackGrad da) ) = DVar normValue (realNode node)
    where normValue = x**2 + y**2
          go :: Double -> Gradient
          go a = (2*a) *^ Gradient x y
          node = ExprSum (da (toDenseBuilder . go))

sqrNorm'' :: forall a. BVar a Vector -> BVar a Double
sqrNorm'' (DVar (Vector x y) dv) = DVar normValue node
    where normValue = x**2 + y**2
          go :: Double -> Gradient
          go a = (2*a) *^ Gradient x y
          node = lift1'' (toDenseBuilder . go) dv


main :: IO ()
main = return ()
