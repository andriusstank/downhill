{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.AD (adTests)
where

import Downhill.Linear.AD (AD, Graph, ad, dense1, identityAD)
import Downhill.Linear.Fun (ScalarMulFun (..), (<*|))
import Downhill.Orphans ()
import NumHask.Prelude
import Test.HUnit ((@=?))
import Test.Hspec (SpecWith, describe, it)
import qualified Downhill.Linear.AD as AD
import Downhill.Linear.Fun ((|*>))

zeroTest :: SpecWith ()
zeroTest = it "zero" $ do
    let
        f :: Graph Int Int
        f = ad zero
    f <*| 0 @=? 0
    0 |*> f @=? 0
    return ()

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

minusTest :: SpecWith ()
minusTest = it "minus" $ do
    let
        f1, f2 :: AD Int Int
        f1 = dense1 (ScalarMulFun 5) identityAD
        f2 = dense1 (ScalarMulFun 3) identityAD
        f :: Graph Int Int
        f = ad (f1 - f2)
    f <*| 7 @=? 14
    7 |*> f @=? 14
    return ()

pairTest1 :: SpecWith ()
pairTest1 = it "join pair" $ do
    let
        f1, f2 :: AD Int Int
        f1 = dense1 (ScalarMulFun 2) identityAD
        f2 = dense1 (ScalarMulFun 3) identityAD
        f3 :: AD Int (Int, Int)
        f3 = AD.T2 f1 f2
        f :: Graph Int (Int, Int)
        f = ad f3
    f <*| 5 @=? (10, 15)
    (5, 7) |*> f @=? 2*5 + 3*7

pairTest2 :: SpecWith ()
pairTest2 = it "join pair" $ do
    let
        f1 :: AD (Int, Int) (Int, Int)
        f1 = identityAD
        f2, f3 :: AD (Int, Int) Int
        AD.T2 f2 f3 = f1
        gFst :: Graph (Int, Int) Int
        gFst = ad f2
        gSnd :: Graph (Int, Int) Int
        gSnd = ad f3
    gFst <*| (3, 5) @=? 3
    7 |*> gFst @=? (7, 0)
    gSnd <*| (3, 5) @=? 5
    11 |*> gSnd @=? (0, 11)

adTests :: SpecWith ()
adTests = describe "AD" $ do
    zeroTest
    mulTest
    sumTest
    minusTest
    pairTest1
    pairTest2
