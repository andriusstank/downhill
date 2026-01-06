{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.GD ()
where

import Downhill.BVar (BVar, DFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold, Tang)
import Downhill.VSpace (InnerSpace, VSpace, sqrNorm)
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
    ) =>
    BVar r v -> BVar r (Scalar v)
bvarLoss1 = loss1

dloss1 :: (Manifold a, HasBuilder a, FromRational a, a ~ Scalar a, a ~ Tang a, DualOf a ~ a) => BVar.DFun2 a (Scalar a) (BVar.Graph (Tang a) (Tang (Scalar a)))
dloss1 = BVar.ad bvarLoss1
