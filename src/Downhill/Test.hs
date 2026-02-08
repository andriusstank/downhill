{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Test (testVSpace, testLinearMap, testLinearBFun, testBilinearBFun) where

import Control.Monad (when)
import Downhill.BVar (BVar (bvarValue), DFun (DFun, unDFun), Graph)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf, LinearMap ((<*|), (|*>)), ScalarMulFun (ScalarMulFun))
import Downhill.Manifold (Manifold, Tang)
import Downhill.VSpace (CScalar, InnerSpace (..), VSpace ((|*|)), conj, (*^))
import Hedgehog (Gen, PropertyT, forAll, (===))
import NumHask.Prelude

{-# ANN testPlus "HLint: ignore Use -" #-}
testPlus :: (Show a, Eq a, Subtractive a) => Gen a -> PropertyT IO ()
testPlus genA = do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x + zero === x
    zero + x === x
    x + negate x === zero
    x + y === y + x
    (x + y) + z === x + (y + z)
    x - y === x + negate y

testMul :: (Show a, Eq a, Subtractive a, Ring a, MultiplicativeAction a) => Gen a -> PropertyT IO ()
testMul genA = do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x * one === x
    one * x === x
    x * y === y * x
    (x * y) * z === x * (y * z)
    x * (y + z) === x * y + x * z

testVector :: (VSpace v, Show (Scalar v), Eq (Scalar v), Show v, Eq v) => Gen (Scalar v) -> Gen v -> PropertyT IO ()
testVector genA genV = do
    a1 <- forAll genA
    a2 <- forAll genA
    v1 <- forAll genV
    v2 <- forAll genV
    v1 |* one === v1
    v1 |* zero === zero
    v1 |* a1 === a1 *| v1
    (a1 * a2) *| v1 === a1 *| (a2 *| v1)
    a1 *| (v1 + v2) === a1 *| v1 + a1 *| v2
    a1 |* a2 === a1 * a2

testHilbert ::
    forall v.
    ( InnerSpace v
    , Show (Scalar v)
    , Eq (Scalar v)
    , Show v
    , Eq v
    , CScalar (Scalar v)
    , Eq (DualOf v)
    , Show (DualOf v)
    ) =>
    Gen (Scalar v) -> Gen v -> PropertyT IO ()
testHilbert genA genV = do
    a <- forAll genA
    v1 <- forAll genV
    v2 <- forAll genV
    v3 <- forAll genV
    conj (conj a) === a
    v1 <.> v2 === conj (v2 <.> v1)
    (v1 + v2) <.> v3 === (v1 <.> v3) + (v2 <.> v3)
    (a *| v1) <.> v2 === a * (v1 <.> v2)
    v1 <.> (a *| v2) === conj a * (v1 <.> v2)

    v1 <.> v2 === riesz v2 |*| v1
    coriesz (riesz v1) === v1
    riesz (v1 + v2) === riesz v1 + riesz v2
    riesz (a *| v1) === conj a *| riesz v1

testVSpace ::
    ( VSpace v
    , Show (Scalar v)
    , Eq (Scalar v)
    , Show v
    , Eq v
    , InnerSpace v
    , Eq
        (DualOf v)
    , Show (DualOf v)
    ) =>
    Gen (Scalar v) -> Gen v -> PropertyT IO ()
testVSpace genA genV = do
    testPlus genA
    testMul genA
    testPlus genV
    testVector genA genA
    testVector genA genV
    testHilbert genA genA
    testHilbert genA genV

testLinearMap ::
    forall f u v.
    ( LinearMap f u v
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
testLinearMap genA genU genDV f = do
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
    forall u v.
    ( Eq v
    , VSpace u
    , VSpace v
    , VSpace (DualOf u)
    , VSpace (DualOf v)
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
    , Show (Scalar v)
    , Show u
    , Show (DualOf v)
    , Show v
    , Show (DualOf u)
    ) =>
    Gen (Scalar v) -> Gen u -> Gen (DualOf v) -> (forall r. BVar r u -> BVar r v) -> PropertyT IO ()
testLinearBFun genScalar genU genDV bf = do
    x <- forAll genU
    let DFun f = BVar.ad bf
        (fx, df) = f x
    testLinearMap genScalar genU genDV df
    fx === df <*| x

testBilinearBFun ::
    ( VSpace u1
    , VSpace u2
    , VSpace v
    , Tang u1 ~ u1
    , Tang v ~ v
    , Tang (Scalar v) ~ Scalar v
    , Scalar u1 ~ Scalar v
    , Scalar u2 ~ Scalar v
    , HasBuilder u1
    , HasBuilder v
    , HasBuilder (DualOf u1)
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    , Manifold (Scalar v)
    , VSpace (DualOf u1)
    , VSpace (DualOf v)
    , Eq v
    , Eq (DualOf u1)
    , Eq (Scalar v)
    , Show v
    , Show (Scalar v)
    , Show (DualOf v)
    , Show (DualOf u1)
    , Show u2
    , Show u1
    , Tang u2 ~ u2
    , Tang (Scalar u2) ~ Scalar u2
    , HasBuilder u2
    , HasBuilder (DualOf u2)
    , HasBuilder (Scalar u2)
    , Manifold (Scalar u2)
    , VSpace (DualOf u2)
    , Eq (DualOf u2)
    , Eq (Scalar u2)
    , Show (Scalar u2)
    , Show (DualOf u2)
    ) =>
    Gen (Scalar v) -> Gen u1 -> Gen u2 -> Gen (DualOf v) -> (forall r. BVar r u1 -> BVar r u2 -> BVar r v) -> PropertyT IO ()
testBilinearBFun genScalar genU1 genU2 genDV bf = do
    u1 <- forAll genU1
    u2 <- forAll genU2
    testLinearBFun genScalar genU2 genDV (bf (BVar.constant u1))
    testLinearBFun genScalar genU1 genDV (`bf` BVar.constant u2)
