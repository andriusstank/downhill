{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module DownhillTest.GD ()
where

import Downhill.Algebra (FrobeniusAlgebra)
import Downhill.BVar (BVar)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf, AsLinearMap (..), Metric, LinearMap (..))
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.Opt.GD (AccGdProblem (..), accGd)
import Downhill.VSpace (InnerSpace (..), RealScalar, VSpace (..), sqrNorm)
import DownhillTest.VSpace.V2 (V2 (..))
import NumHask.Prelude

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
    , FrobeniusAlgebra (Scalar v)
    ) =>
    BVar r v -> BVar r (Scalar v)
bvarLoss1 = loss1

dloss1 ::
    forall v.
    ( InnerSpace v
    , Tang v ~ v
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
    , FrobeniusAlgebra (Scalar v)
    ) =>
    BVar.DFun v (Scalar v)
dloss1 = BVar.ad bvarLoss1

data L2 = L2

instance (v ~ DualOf dv, dv ~ DualOf v, InnerSpace v) => AsLinearMap L2 v dv where
    toLinearMap L2 = LinearMap riesz riesz

instance (InnerSpace v) => Metric L2 v

problem :: AccGdProblem (V2 Double) Double L2
problem =
    AccGdProblem
        { loss = dloss1 @(V2 Double)
        , metric = L2
        , momentum = Nothing
        , dy = 1.0
        }

solution = accGd problem (V2 2 3)

-- >>> Stream.take 3 solution
-- [(V2 2.0 3.0,6.5),(V2 0.0 0.0,0.0),(V2 0.0 0.0,0.0)]
