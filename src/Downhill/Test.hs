{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Test (testLinearFun, testLinearBFun) where

import Control.Monad (when)
import Downhill.BVar (BVar (bvarValue), DFun (DFun, unDFun), Graph)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf, LinearFun ((<*|), (|*>)), ScalarMulFun (ScalarMulFun))
import Downhill.Manifold (Manifold, Tang)
import Downhill.VSpace (VSpace ((|*|)), (*^))
import Hedgehog (Gen, PropertyT, forAll, (===))
import NumHask.Prelude

testLinearFun ::
    forall f u v.
    ( LinearFun f u v
    , Scalar u ~ Scalar v
    , Eq (Scalar v)
    , Show (Scalar v)
    , Show u
    , Show (DualOf v)
    , Show (DualOf u)
    , Eq (DualOf u)
    , Eq v
    , Show v
    , VSpace v
    , VSpace u
    ) =>
    Gen (Scalar u) -> Gen u -> Gen (DualOf v) -> f -> PropertyT IO ()
testLinearFun genA genU genDV f = do
    do
        a <- forAll genA
        u <- forAll genU
        dv <- forAll genDV
        f <*| (a *| u) === a *| (f <*| u)
        (a *| dv) |*> f === a *| (dv |*> f)
        dv |*| (f <*| u) === (dv |*> f) |*| u
    do
        u1 <- forAll genU
        u2 <- forAll genU
        f <*| (u1 + u2) === f <*| u1 + f <*| u2
        v1 <- forAll genDV
        v2 <- forAll genDV
        (v1 + v2) |*> f === v1 |*> f + v2 |*> f

testLinearBFun ::
    forall m u v.
    ( Eq v
    , VSpace u
    , VSpace v
    , VSpace (DualOf u)
    , VSpace (DualOf v)
    , MonadFail m
    , HasBuilder u
    , HasBuilder v
    , HasBuilder (DualOf u)
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar u)
    , Scalar u ~ Scalar v
    , Tang v ~ v
    , Tang u ~ u
    , Tang (Scalar v) ~ Scalar v
    , Manifold (Scalar v)
    , Eq (DualOf u)
    , Eq (Scalar v)
    , m ~ PropertyT IO
    , Show (Scalar v)
    , Show u
    , Show (DualOf v)
    , Show v, Show (DualOf u)
    ) =>
    Gen (Scalar v) -> Gen u -> Gen (DualOf v) -> (forall r. BVar r u -> BVar r v) -> m ()
testLinearBFun genScalar genU genDV bf = do
    x <- forAll genU
    let DFun f = BVar.ad bf
        (fx, df) = f x
    testLinearFun genScalar genU genDV df
    fx === df <*| x

{-
testAdditive :: (Eq a, Additive a, MonadFail m) => m a -> m ()
testAdditive gen = do
    x <- gen
    y <- gen
    z <- gen
    when ((x+y)+z /= x+(y+z)) $
        fail "(x+y)+z /= x+(y+z)"
-}