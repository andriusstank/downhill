{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.AD (adTests)
where

import Downhill.Linear.AD (AD, Graph, ad, dense1, identityAD)
import Downhill.Linear.Fun (LinearFun (..), ScalarMulFun (..))
import Downhill.Orphans ()
import NumHask.Prelude
import Test.HUnit ((@=?))
import Test.Hspec (SpecWith, describe, it)

zeroTest :: SpecWith ()
zeroTest = it "zero" $ do
    let
        f :: Graph Int Int
        f = ad zero
    f <*| 0 @=? 0
    0 |*> f @=? 0

mulTest :: SpecWith ()
mulTest = it "mul" $ do
    let
        f :: Graph Int Int
        f = ad (dense1 (ScalarMulFun 2) identityAD)
    f <*| 3 @=? 6
    3 |*> f @=? 6

sumTest :: SpecWith ()
sumTest = it "sum" $ do
    let
        f1, f2 :: AD Int Int
        f1 = dense1 (ScalarMulFun 2) identityAD
        f2 = dense1 (ScalarMulFun 3) identityAD
        f :: Graph Int Int
        f = ad (f1 + f2)
    f <*| 7 @=? 35
    7 |*> f @=? 35
    return ()

adTests :: SpecWith ()
adTests = describe "AD" $ do
    zeroTest
    mulTest
    sumTest
