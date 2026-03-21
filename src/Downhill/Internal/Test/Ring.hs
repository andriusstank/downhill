{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Internal.Test.Ring (testAdditive, testRing)
where

import Downhill.Linear.Fun (DualOf)
import Downhill.VSpace (CScalar (conj), InnerSpace (coriesz, riesz, (<.>)), (|*|))
import Hedgehog (Gen, forAll, property, (===))
import NumHask.Prelude
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

{-# ANN testAdditive ("HLint: ignore Use -" :: String) #-}
{-# ANN testAdditive ("HLint: ignore Redundant negate" :: String) #-}
testAdditive :: forall a. (Show a, Subtractive a, Eq a) => Gen a -> TestTree
testAdditive genA = testGroup "Additive" [plusTest, minusTest]
  where
    plusTest :: TestTree
    plusTest = testProperty "Plus" . property $ do
        x <- forAll genA
        y <- forAll genA
        z <- forAll genA
        x + zero === x
        zero + x === x
        x + y === y + x
        (x + y) + z === x + (y + z)

    minusTest :: TestTree
    minusTest = testProperty "Minus" . property $ do
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

testMul ::
    (InnerSpace a, Multiplicative a, Show a, Eq a, Eq (Scalar a), Show (Scalar a), DualOf a ~ a) =>
    Gen a -> TestTree
testMul gen = testProperty "Mul" . property $ do
    a <- forAll gen
    b <- forAll gen
    c <- forAll gen
    a |*| (b * c) === (a * b) |*| c

testInnerProduct ::
    (InnerSpace a, Multiplicative a, Show a, Eq (Scalar a), Show (Scalar a), Eq a, Eq (DualOf a), Multiplicative (DualOf a), Show (DualOf a)) =>
    Gen a -> TestTree
testInnerProduct gen = testProperty "Inner product" . property $ do
    a <- forAll gen
    b <- forAll gen
    a <.> b === conj (b <.> a)
    a <.> b === riesz b |*| a
    coriesz (riesz a) === a
    riesz (a * b) === riesz b * riesz a
    riesz (a + b) === riesz a + riesz b

testRing ::
    (Show a, Eq a, Ring a, InnerSpace a, DualOf a ~ a, Eq (Scalar a), Show (Scalar a)) =>
    TestName -> Gen a -> TestTree
testRing name genA =
    testGroup
        name
        [ testAdditive genA
        , testMultiplicative genA
        , testDistributive genA
        , testMul genA
        , testInnerProduct genA
        ]
