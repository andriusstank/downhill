{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Test (
    testVSpace,
    testHilbertSpace,
    testLinearMap,
    {-
    testLinearBFun,
    testBilinearBFun,
    testHilbertMap,-}
) where

import Downhill.Linear.Fun (DualOf, LinearMap ((<*|), (|*>)))
import Downhill.VSpace (CScalar, InnerSpace (..), VSpace ((|*|)), conj)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Internal.Property (property)
import NumHask.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

{-# ANN testAdditive "HLint: ignore Use -" #-}
testAdditive :: (Show a, Eq a, Additive a) => Gen a -> TestTree
testAdditive genA = testProperty "Additive" . property $ do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x + zero === x
    zero + x === x
    x + y === y + x
    (x + y) + z === x + (y + z)

{-# ANN testSubtractive "HLint: ignore Use -" #-}
{-# ANN testSubtractive "HLint: ignore Redundant negate" #-}
testSubtractive :: forall a. (Show a, Eq a, Subtractive a) => Gen a -> TestTree
testSubtractive genA = testProperty "Subtractive" . property $ do
    x <- forAll genA
    y <- forAll genA
    x - y === x + negate y
    x + negate x === zero
    negate (negate x) === x
    negate @a zero === zero

testMultiplicative :: (Show a, Eq a, Multiplicative a) => Gen a -> TestTree
testMultiplicative genA = testProperty "Multiplicative" . property $ do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x * one === x
    one * x === x
    x * y === y * x
    (x * y) * z === x * (y * z)

testDistributive :: (Show a, Eq a, Additive a, Multiplicative a) => Gen a -> TestTree
testDistributive genA = testProperty "Distributive" . property $ do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x * (y + z) === x * y + y * z

testRing :: (Show a, Eq a, Ring a) => Gen a -> TestTree
testRing genA =
    testGroup
        "Ring"
        [ testAdditive genA
        , testSubtractive genA
        , testMultiplicative genA
        , testDistributive genA
        ]

testLinearity ::
    ( a ~ Scalar u
    , a ~ Scalar v
    , Show a
    , Show u
    , Eq v
    , MultiplicativeAction u
    , Additive u
    , Show v
    , MultiplicativeAction v
    , Additive v
    ) =>
    Gen a -> Gen u -> (u -> v) -> TestTree
testLinearity genA genU f = testProperty "Linearity" . property $ do
    a <- forAll genA
    u <- forAll genU
    f (a *| u) === a *| f u
    v <- forAll genU
    f (u + v) === f u + f v

testLeftLinearity ::
    ( a ~ Scalar u
    , a ~ Scalar v
    , a ~ Scalar w
    , Show a
    , Show u
    , Show v
    , Show w
    , Eq v
    , Eq w
    , Additive u
    , Additive v
    , MultiplicativeAction u
    , MultiplicativeAction v
    , MultiplicativeAction w, Additive w
    ) =>
    Gen a -> Gen u -> Gen v -> (u -> v -> w) -> TestTree
testLeftLinearity genA genU genV f = testProperty "Left Linearity" . property $ do
    a <- forAll genA
    u <- forAll genU
    v <- forAll genV
    (a *| u) `f` v === a *| (u `f` v)
    u2 <- forAll genU
    (u + u2) `f` v === u `f` v + u2 `f` v

testRightLinearity ::
    ( a ~ Scalar u
    , a ~ Scalar v
    , a ~ Scalar w
    , Show a
    , Show u
    , Show v
    , Eq v
    , Eq w
    , Additive u
    , Additive v
    , Show w
    , MultiplicativeAction u
    , MultiplicativeAction v
    , MultiplicativeAction w
    , Additive w
    ) =>
    Gen a -> Gen u -> Gen v -> (u -> v -> w) -> TestTree
testRightLinearity genA genU genV f = testProperty "Right Linearity" . property $ do
    a <- forAll genA
    u <- forAll genU
    v <- forAll genV
    u `f` (a *| v) === a *| (u `f` v)
    v2 <- forAll genV
    u `f` (v + v2) === u `f` v2 + u `f` v

testBilinearity ::
    ( a ~ Scalar u
    , a ~ Scalar v
    , a ~ Scalar w
    , Show a
    , Show u
    , MultiplicativeAction u
    , Additive u
    , Additive w
    , Show v
    , Show w
    , Eq v
    , Eq w
    , Additive v
    , MultiplicativeAction v
    , MultiplicativeAction w
    ) =>
    Gen a ->
    Gen u ->
    Gen v ->
    (u -> v -> w) ->
    TestTree
testBilinearity genA genU genV f = testGroup "Bilinearity" [testLeftLinearity genA genU genV f, testRightLinearity genA genU genV f]

testScalarVectorMul :: forall v. (VSpace v, Show (Scalar v), Eq (Scalar v), Show v, Eq v) => Gen (Scalar v) -> Gen v -> TestTree
testScalarVectorMul genA genV =
    testGroup
        "Scalar vector multiplication"
        [ simple
        , testBilinearity genA genA genA (*|)
        , testBilinearity genA genA genV (*|)
        ]
  where
    simple = testProperty "Simple checks" . property $ do
        a <- forAll genA
        v <- forAll genV
        v |* one === v
        v |* zero === zero
        v |* a === a *| v

testVSpace ::
    (VSpace v, Show (Scalar v), Eq (Scalar v), Show v, Eq v, Show (DualOf v)) =>
    Gen (Scalar v) -> Gen v -> Gen (DualOf v) -> TestTree
testVSpace genA genV genDv =
    testGroup
        "VSpace"
        [ testRing genA
        , testAdditive genV
        , testSubtractive genV
        , testScalarVectorMul genA genV
        , testBilinearity genA genDv genV (|*|)
        ]

testInnerProduct ::
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
    Gen (Scalar v) -> Gen v -> TestTree
testInnerProduct genA genV = testProperty "Inner product" . property $ do
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

testHilbertSpace ::
    ( VSpace v
    , Show (Scalar v)
    , Eq (Scalar v)
    , Show v
    , Eq v
    , InnerSpace v
    , Eq (DualOf v)
    , Show (DualOf v)
    ) =>
    Gen (Scalar v) -> Gen v -> Gen (DualOf v) -> TestTree
testHilbertSpace genA genV genDv =
    testGroup
        "Hilber space"
        [ testVSpace genA genV genDv
        , testInnerProduct genA genA
        , testInnerProduct genA genV
        ]

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
    Gen (Scalar u) -> Gen u -> Gen (DualOf v) -> f -> TestTree
testLinearMap genA genU genDV f = testGroup "Linear Map" [
    testLinearity genA genU (f <*|),
    testLinearity genA genDV (|*> f),
    testAdjoint
  ]
  where
    testAdjoint = testProperty "Adjoint" . property $ do
        u <- forAll genU
        v <- forAll genDV
        (v |*> f) |*| u === v |*| (f <*| u)

{-
testHilbertMap ::
    forall f u v.
    ( VSpace v
    , VSpace u
    , VSpace f
    , LinearMap f u v
    , LinearMap (DualOf f) v u
    , Show u
    , Show v
    , Eq v
    , Eq (DualOf v)
    , InnerSpace v
    , Show (DualOf v)
    , InnerSpace u
    , InnerSpace f
    , Show f
    ) =>
    Gen u -> Gen f -> PropertyT IO ()
testHilbertMap genU genF = do
    x <- forAll genU
    f <- forAll genF
    riesz (f <*| x) === riesz x |*> riesz f

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
-}