{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module DownhillTest.GD ()
where

import Downhill.BVar qualified as BVar
import Downhill.Linear.Fun (DualOf, Metric, LinearFun(..))
import Downhill.Manifold (Manifold(..), Tang, IsScalar)
import Downhill.VSpace (InnerSpace(..), VSpace (..), sqrNorm, RealScalar)
import NumHask.Prelude
import Downhill.Linear.Builder (HasBuilder, Dense(..))
import Downhill.Opt.GD (AccGdProblem (..), accGd)
import Downhill.BVar (BVar, someDFun, SomeDFun(SomeDFun))
import Data.Stream.Infinite qualified as Stream

loss1 :: (InnerSpace v, FromRational (Scalar v)) => v -> Scalar v
loss1 x = 0.5 * sqrNorm x

bvarLoss1 ::
    forall r v.
    ( InnerSpace v
    , VSpace (DualOf v)
    , Manifold v
    , Manifold (Scalar v)
    , Tang v ~ v
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    , Fractional (Scalar v)
    , FromRational (Scalar v)
    , RealScalar (Scalar v)
    ) =>
    BVar r v -> BVar r (Scalar v)
bvarLoss1 = loss1

dloss1 :: forall v.
    ( InnerSpace v, Tang v ~ v
    , Tang (Scalar v) ~ Scalar v
    , Tang (DualOf v) ~ DualOf v
    , Manifold v
    , Manifold (Scalar v)
    , VSpace (DualOf v)
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    , Fractional (Scalar v)
    , FromRational (Scalar v)
    , RealScalar (Scalar v)
    ) =>
    BVar.DFun v (Scalar v) (BVar.Graph v (Scalar v))
dloss1 = BVar.ad bvarLoss1

data Vec2 a = Vec2 a a
    deriving (Generic, Show)
    deriving (Additive, Subtractive, Multiplicative, MultiplicativeAction) via Generically (Vec2 a)
    deriving HasBuilder via (Dense (Vec2 a))

deriving via Generically (Vec2 a) instance (Scalar a ~ a, VSpace a) => VSpace (Vec2 a)
deriving via Generically (Cov2 a) instance (Scalar a ~ a, VSpace a) => VSpace (Cov2 a)
deriving via Generically (Vec2 a) instance (Scalar a ~ a, Tang a ~ a, DualOf a ~ a, Manifold a) => Manifold (Vec2 a)

data Cov2 a = Cov2 a a
    deriving (Generic, Show)
    deriving (Additive, Subtractive, Multiplicative, MultiplicativeAction) via Generically (Cov2 a)
    deriving HasBuilder via (Dense (Cov2 a))

type instance Tang (Vec2 a) = Vec2 a
type instance Tang (Cov2 a) = Cov2 a
type instance DualOf (Vec2 a) = Cov2 a
type instance DualOf (Cov2 a) = Vec2 a

instance (IsScalar a) => InnerSpace (Vec2 a) where
    riesz (Vec2 x y) = Cov2 x y
    coriesz (Cov2 x y) = Vec2 x y
    Vec2 x1 y1 <.> Vec2 x2 y2 = x1*x2 + y1*y2

instance (IsScalar a) => InnerSpace (Cov2 a) where
    riesz (Cov2 x y) = Vec2 x y
    coriesz (Vec2 x y) = Cov2 x y
    Cov2 x1 y1 <.> Cov2 x2 y2 = x1*x2 + y1*y2

data L2 = L2

instance (v ~ DualOf dv, dv ~ DualOf v, InnerSpace v) => LinearFun L2 v dv where
  L2 <*| v = riesz v
  v |*> L2 = riesz v

instance InnerSpace v => Metric L2 v where

problem :: AccGdProblem (Vec2 Double) Double L2
problem = AccGdProblem {
    loss = someDFun (dloss1 @(Vec2 Double)),
    metric = L2,
    momentum = Nothing,
    dy = 1.0
}

solution = accGd problem (Vec2 2 3)

-- >>> Stream.take 3 solution
-- [(Vec2 2.0 3.0,6.5),(Vec2 0.0 0.0,0.0),(Vec2 0.0 0.0,0.0)]
