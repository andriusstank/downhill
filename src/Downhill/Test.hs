{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Test (
    testRing,
    testVSpace,
    testHilbertSpace,
    testLinearMap,
    {-
    testVSpace,
    testHilbertSpace,
    testLinearMap,
    testHilbertMap,
    -}
    {-
    testLinearBFun,
    testBilinearBFun,
    testHilbertMap,-}
) where

import Control.Arrow (ArrowChoice (right))
import Downhill.Linear.Fun (DualOf, LinearMap ((<*|), (|*>)))
import Downhill.VSpace (CScalar, InnerSpace (..), VSpace ((|*|)), conj)
import Hedgehog (Gen, Group (..), MonadTest, Property, PropertyT, annotateShow, forAll, forAllWith, property, (===))
import NumHask.Prelude hiding (Group)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

{-# ANN testAdditive ("HLint: ignore Use -" :: String) #-}
testAdditive :: forall a. (Eq a, Additive a, Show a) => Gen a -> TestTree
testAdditive genA = testProperty "Additive" . property $ do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x + zero === x
    zero + x === x
    x + y === y + x
    (x + y) + z === x + (y + z)

{-# ANN testSubtractive ("HLint: ignore Use -" :: String) #-}
{-# ANN testSubtractive ("HLint: ignore Redundant negate" :: String) #-}
testSubtractive :: forall a. (Eq a, Subtractive a, Show a) => Gen a -> TestTree
testSubtractive genA = testProperty "Subtractive" . property $ do
    x <- forAll genA
    y <- forAll genA
    x - y === x + negate y
    x + negate x === zero
    negate (negate x) === x
    negate @a zero === zero

testMultiplicative :: (Show a, Eq a, Multiplicative a, InnerSpace a, Multiplicative (DualOf a)) => Gen a -> TestTree
testMultiplicative genA = testProperty "Multiplicative" . property $ do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x * one === x
    one * x === x
    y * x === coriesz (riesz x * riesz y)
    (x * y) * z === x * (y * z)

testDistributive :: (Show a, Eq a, Additive a, Multiplicative a) => Gen a -> TestTree
testDistributive genA = testProperty "Distributive" . property $ do
    x <- forAll genA
    y <- forAll genA
    z <- forAll genA
    x * (y + z) === x * y + x * z

testRing :: (Show a, Eq a, Ring a, Multiplicative (DualOf a), InnerSpace a ) => TestName -> Gen a -> TestTree
testRing name genA =
    testGroup
        name
        [ testAdditive genA
        , testSubtractive genA
        , testMultiplicative genA
        , testDistributive genA
        ]

testVectorSpace ::
    ( a ~ Scalar v
    , a ~ Scalar a
    , Eq a
    , Eq v
    , Show a
    , Show v
    , MultiplicativeAction v
    , MultiplicativeAction a
    , Subtractive v
    , Ring a
    , InnerSpace a
    ) =>
    Gen a -> Gen v -> TestTree
testVectorSpace genA genV =
    testGroup
        "VSpace"
        [ testRing "Scalar ring" genA
        , testAdditive genV
        , testSubtractive genV
        , oneMul
        , scalarVSpace
        , multCompat
        , multBilinear
        , flipm
        ]
  where
    oneMul = testProperty "Mult by 1" . property $ do
        v <- forAll genV
        one *| v === v
    scalarVSpace = testProperty "Scalar vector space" . property $ do
        a <- forAll genA
        b <- forAll genA
        a *| b === a * b
    multCompat = testProperty "Scalar multiplication compatibility" . property $ do
        a <- forAll genA
        b <- forAll genA
        v <- forAll genV
        a *| (b *| v) === (a * b) *| v
    multBilinear = testBilinearity "Bilinearity of *|" genA genA genV (pure (*|))
    flipm = testProperty "|*" . property $ do
        a <- forAll genA
        v <- forAll genV
        a *| v === v |* a

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
    TestName -> Gen a -> Gen u -> PropertyT IO (u -> v) -> TestTree
testLinearity name genA genU genF = testGroup name [homogeneity, additivity]
  where
    homogeneity = testProperty "Homogeneity" . property $ do
        f <- genF
        a <- forAll genA
        u <- forAll genU
        f (a *| u) === a *| f u
    additivity = testProperty "Additivity" . property $ do
        f <- genF
        u <- forAll genU
        v <- forAll genU
        f (u + v) === f u + f v

propAntiLinearity ::
    ( a ~ Scalar u
    , a ~ Scalar v
    , CScalar a
    , Show a
    , Show u
    , Eq v
    , MultiplicativeAction u
    , Additive u
    , Show v
    , MultiplicativeAction v
    , Additive v
    ) =>
    TestName -> Gen a -> Gen u -> PropertyT IO (u -> v) -> TestTree
propAntiLinearity name genA genU genF = testGroup name [homogeneity, additivity]
  where
    homogeneity = testProperty "Homogeneity" . property $ do
        f <- genF
        a <- forAll genA
        u <- forAll genU
        f (a *| u) === conj a *| f u
    additivity = testProperty "Additivity" . property $ do
        f <- genF
        u <- forAll genU
        v <- forAll genU
        f (u + v) === f u + f v

testBilinearity ::
    forall u v w a.
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
    TestName ->
    Gen a ->
    Gen u ->
    Gen v ->
    PropertyT IO (u -> v -> w) ->
    TestTree
testBilinearity name genA genU genV genF = testGroup name [leftLinearity, rightLinearity]
  where
    bindLeft :: PropertyT IO (v -> w)
    bindLeft = do
        f <- genF
        u <- forAll genU
        return (u `f`)
    bindRight :: PropertyT IO (u -> w)
    bindRight = do
        f <- genF
        v <- forAll genV
        return (`f` v)
    rightLinearity = testLinearity "Right" genA genV bindLeft
    leftLinearity = testLinearity "Left" genA genU bindRight

testVSpace ::
    (VSpace v, Show (Scalar v), Eq (Scalar v), Show v, Eq v, Show (DualOf v)) =>
    Gen (Scalar v) -> Gen v -> Gen (DualOf v) -> TestTree
testVSpace genA genV genDv =
    testGroup
        "VSpace"
        [ testVectorSpace genA genV
        , testBilinearity "Bilinearity of |*|" genA genDv genV (pure (|*|))
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
    TestName -> Gen (Scalar v) -> Gen v -> Gen (DualOf v) -> TestTree
testHilbertSpace name genA genV genDv =
    testGroup
        name
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
    , Show f
    ) =>
    TestName -> Gen (Scalar u) -> Gen u -> Gen (DualOf v) -> Gen f -> TestTree
testLinearMap name genA genU genDV genF =
    testGroup
        name
        [ testLinearity "Left" genA genU leftF
        , testLinearity "Right" genA genDV rightF
        , testAdjoint
        ]
  where
    leftF :: PropertyT IO (u -> v)
    leftF = do
        f <- forAll genF
        return (f <*|)
    rightF :: PropertyT IO (DualOf v -> DualOf u)
    rightF = do
        f <- forAll genF
        return (|*> f)
    testAdjoint = testProperty "Adjoint" . property $ do
        u <- forAll genU
        v <- forAll genDV
        f <- forAll genF
        (v |*> f) |*| u === v |*| (f <*| u)

{-
testHilbertMap ::
    forall f a u v.
    ( VSpace v
    , VSpace u
    , VSpace f
    , Scalar u ~ a
    , Scalar v ~ a
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
    , Show a
    , Show (DualOf u)
    , Eq a
    , Eq (DualOf u)
    ) =>
    Gen a -> Gen u -> Gen (DualOf v) -> Gen f -> TestTree
testHilbertMap genA genU genDV genF = testGroup "Hilbert Linear Map" [testLinearMap genA genU genDV genF, testAdjoint]
  where
    testAdjoint = testProperty "Adjoint" . property $ do
        x <- forAll genU
        f <- forAll genF
        riesz (f <*| x) === riesz x |*> riesz f

{-
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
-}