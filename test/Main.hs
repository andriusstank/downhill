{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import DownhillTest.AD (adTests)
import DownhillTest.Diff (diffTests)
import DownhillTest.Record (recordTests)
import DownhillTest.Tree (treeTests)
import Test.Hspec (hspec, describe, it)

import Downhill.BVar (BVar)
import Downhill.Linear.Fun (LinearMap (..), ScalarMulFun (..))
import Downhill.Test (testVSpace, testHilbertSpace, testLinearMap, testRing)
import DownhillTest.V2 (M2 (..), V2 (..), Cov2 (Cov2))
import Hedgehog
import Hedgehog (Gen, Property, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import NumHask.Prelude
import Test.HUnit ((@=?), Assertion)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty(TestTree, defaultMain, testGroup)
import GHC.RTS.Flags (MiscFlags(generateStackTrace))

testFun :: BVar r Int -> BVar r Int
testFun x = x

genInt :: Gen Integer
genInt = Gen.integral (Range.linearFrom 0 (-100) 100)

genV2 :: Gen a -> Gen (V2 a)
genV2 g = V2 <$> g <*> g

genCov2 :: Gen a -> Gen (Cov2 a)
genCov2 g = Cov2 <$> g <*> g

genCInt :: Gen (Complex Integer)
genCInt = Complex <$> ((,) <$> genInt <*> genInt)

genM2 :: Gen a -> Gen (M2 a)
genM2 g = M2 <$> g <*> g <*> g <*> g

{-
testM2 :: [(PropertyName, Property)]
testM2 =
    [
        ( "testM2"
        , property
            $ do
                f1 <- forAll (genM2 genInt)
                testLinearMap genInt (genV2 genInt) (Transpose <$> genV2 genInt) f1
                testHilbertMap (genV2 genInt) (genM2 genInt)
        )
    ]

testM2C :: [(PropertyName, Property)]
testM2C =
    [
        ( "testM2C linear map"
        , property $ do
            f2 <- forAll (genM2 genCInt)
            testLinearMap genCInt (genV2 genCInt) (Transpose <$> genV2 genCInt) f2
        )
    ,
        ( "testM2C hilbert"
        , property $ do
            testHilbertMap (genV2 genCInt) (genM2 genCInt)
        )
    ]

testProp :: [(PropertyName, Property)]
testProp = [("test", property go)]
    where
        go = do
                x <- forAll genInt
                y <- forAll genInt
                assert (x+y<100)

-}

mainHg :: TestTree
mainHg = testGroup "All" [
        testHilbertSpace "(V2 Int) Hilbert space" genInt (genV2 genInt) (genCov2 genInt),
        testHilbertSpace "(V2 (Complex Int)) Hilbert space" genCInt (genV2 genCInt) (genCov2 genCInt),
        testHilbertSpace "(M2 (Complex Int)) Hilbert space" genCInt (genM2 genCInt) (genM2 genCInt),
        testRing "Int M2 ring" (genM2 genInt),
        --testRing "Complex M2 ring" (genM2 genCInt),
        testLinearMap "M2 Linear map" genCInt (genV2 genCInt) (genCov2 genCInt) (genM2 genCInt)
        {- , ("vspace v2 cint", testVSpace genCInt (genV2 genCInt))
        , ("vspace m2 int", testVSpace genInt (genM2 genInt))
        , ("vspace m2 cint", testVSpace genCInt (genM2 genCInt)) -}
        ]
        {-
            ++ testM2
            ++ testM2C
            ++ testProp -}
  
mainSpec :: IO ()
mainSpec = hspec $ do
    adTests
    diffTests
    recordTests
    treeTests

main :: IO ()
main = mainSpec >> defaultMain mainHg
