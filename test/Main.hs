{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import DownhillTest.AD (adTests)
import DownhillTest.Diff (diffTests)
import DownhillTest.Record (recordTests)
import DownhillTest.Tree (treeTests)
import Test.Hspec (hspec)

import Downhill.BVar (BVar)
import Downhill.Linear.Fun (LinearMap (..), ScalarMulFun (..))
import Downhill.Test (testLinearBFun, testVSpace, testLinearMap, testHilbertMap)
import DownhillTest.V2 (V2 (..), M2(..))
import Hedgehog
import Hedgehog (Gen, Property, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main (defaultMain)
import Hedgehog.Range qualified as Range
import NumHask.Prelude
import DownhillTest.Transpose (Transpose(..))

testFun :: BVar r Int -> BVar r Int
testFun x = x

genInt :: Gen Integer
genInt = Gen.integral (Range.linearFrom 0 (-100) 100)

genV2 :: Gen a -> Gen (V2 a)
genV2 g = V2 <$> g <*> g

genCInt :: Gen (Complex Integer)
genCInt = Complex <$> ((,) <$> genInt <*> genInt)

genM2 :: Gen a -> Gen (M2 a)
genM2 g = M2 <$> g <*> g <*> g <*> g

mainHg :: Property
mainHg = property $ do
    testVSpace genInt (genV2 genInt)
    testVSpace genCInt (genV2 genCInt)
    testVSpace genInt (genM2 genInt)
    testVSpace genCInt (genM2 genCInt)
    f1 <- forAll (genM2 genInt)
    testLinearMap genInt (genV2 genInt) (Transpose <$> genV2 genInt) f1
    f2 <- forAll (genM2 genCInt)
    testLinearMap genCInt (genV2 genCInt) (Transpose <$> genV2 genCInt) f2
    testHilbertMap (genV2 genInt) f1

mainSpec :: IO ()
mainSpec = hspec $ do
    adTests
    diffTests
    recordTests
    treeTests

main :: IO ()
main = mainSpec >> defaultMain [check mainHg]
