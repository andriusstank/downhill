{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Internal.Test.Ring (
    checkTested,
    checkTestedProp,
    AdditiveTested,
    SubtractiveTested,
    MultiplicativeTested,
    DistributiveTested,
    RingTested,
    testAdditive4,
    testSubtractive4,
    testMultiplicative4,
    testDistributive4,
    testRing4,
)
where

import Data.Constraint (Dict (Dict))
import Data.Reflection (Given (given), give)
import Hedgehog (Gen, Group (Group), Property, checkSequential, forAll, property, (===))
import Hedgehog.Internal.Runner (check)
import NumHask.Prelude hiding (Group)
import Test.HUnit (assertFailure)
import Test.Hspec (it)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Hedgehog (hedgehog)

checkTestedProp :: forall witness. () => witness -> Property -> IO (Dict (Given witness))
checkTestedProp test props = do
    success <- check props
    case success of
        True -> return (give test Dict)
        False -> assertFailure "Failed"

checkTested :: forall witness. () => witness -> Group -> IO (Dict (Given witness))
checkTested test props = do
    success <- checkSequential props
    case success of
        True -> return (give test Dict)
        False -> assertFailure "Failed"

data AdditiveWitness a = AdditiveWitness
type AdditiveTested a = Given (AdditiveWitness a)

testAdditive4 :: forall a. (Show a, Additive a, Eq a, Given (Gen a)) => SpecM () (Dict (AdditiveTested a))
testAdditive4 = do
    let gen = given :: Gen a
    it "x + 0 = x" $ hedgehog $ do
        x <- forAll gen
        x + zero === x
        zero + x === x
    it "x + y = y + x" $ hedgehog $ do
        x <- forAll gen
        y <- forAll gen
        x + y === y + x
    it "(x + y) + z = x + (y + z)" $ hedgehog $ do
        x <- forAll gen
        y <- forAll gen
        z <- forAll gen
        (x + y) + z === x + (y + z)
    return (give (AdditiveWitness @a) Dict)

data SubtractiveWitness (a :: Type) = SubtractiveWitness
type SubtractiveTested a = (AdditiveTested a, Given (SubtractiveWitness a))

{-# ANN testSubtractive ("HLint: ignore Redundant negate" :: String) #-}
{-# ANN testSubtractive ("HLint: ignore Use -" :: String) #-}
testSubtractive :: forall a. (Given (Gen a), Show a, Subtractive a, Eq a, AdditiveTested a) => IO (Dict (SubtractiveTested a))
testSubtractive = do
    Dict <- checkTested (SubtractiveWitness @a) testProps
    return Dict
  where
    genA = given :: Gen a
    testProps =
        Group
            "Additive"
            [ ("Minus", prop_minus)
            , ("Negate", prop_negate)
            ]
    prop_minus = property $ do
        x <- forAll genA
        y <- forAll genA
        x - y === x + negate y
    prop_negate = property $ do
        x <- forAll genA
        x + negate x === zero
        negate (negate x) === x
        negate @a zero === zero

{-# ANN testSubtractive4 ("HLint: ignore Redundant negate" :: String) #-}
{-# ANN testSubtractive4 ("HLint: ignore Use -" :: String) #-}
testSubtractive4 :: forall a. (Given (Gen a), Show a, Subtractive a, Eq a, AdditiveTested a) => SpecM () (Dict (SubtractiveTested a))
testSubtractive4 = do
    let genA = given :: Gen a
    it "x - y = x + (-y)" $ hedgehog $ do
        x <- forAll genA
        y <- forAll genA
        x - y === x + negate y
    it "x + (-x) = 0" $ hedgehog $ do
        x <- forAll genA
        x + negate x === zero
        negate (negate x) === x
        negate @a zero === zero
    return (give (SubtractiveWitness @a) Dict)

data MultiplicativeWitness (a :: Type) = MultiplicativeWitness
type MultiplicativeTested a = Given (MultiplicativeWitness a)

testMultiplicative4 :: forall a. (Given (Gen a), Show a, Multiplicative a, Eq a) => SpecM () (Dict (MultiplicativeTested a))
testMultiplicative4 = do
    let genA = given :: Gen a
    it "1 * x = x" $ hedgehog $ do
        x <- forAll genA
        x * one === x
        one * x === x
    {-
    it "x * y = y * x" $ hedgehog $ do
        x <- forAll genA
        y <- forAll genA
        x * y === y * x
    -}
    it "(x * y) * z === x * (y * z)" $ hedgehog $ do
        x <- forAll genA
        y <- forAll genA
        z <- forAll genA
        (x * y) * z === x * (y * z)
    return (give (MultiplicativeWitness @a) Dict)

data DistributiveWitness (a :: Type) = DistributiveWitness
type DistributiveTested a = Given (DistributiveWitness a)

testDistributive4 :: forall a. (Given (Gen a), Show a, Distributive a, Eq a) => SpecM () (Dict (DistributiveTested a))
testDistributive4 = do
    let genA = given :: Gen a
    it "x * (y + z) === x * y + x * z" $ hedgehog $ do
        x <- forAll genA
        y <- forAll genA
        z <- forAll genA
        x * (y + z) === x * y + x * z
    return (give (DistributiveWitness @a) Dict)

type RingTested a = (DistributiveTested a, SubtractiveTested a)

testRing4 :: forall a. (Show a, Eq a, Ring a, Given (Gen a)) => SpecM () (Dict (RingTested a))
testRing4 = do
    Dict <- testAdditive4 @a
    Dict <- testMultiplicative4 @a
    Dict <- testDistributive4 @a
    Dict <- testSubtractive4 @a
    return Dict
