{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module DownhillTest.RegularRecord.Fields (fieldsTests) where

import Data.Singletons (SingI (..), TyCon1)
import Downhill.BVar (BVar)
import Downhill.BVar qualified as BVar
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.HListOf (HListOf (..))
import DownhillTest.RegularRecord.Fixtures (SplitLeaf, SplitLeafShape (..))
import DownhillTest.VSpace.V2 (V2 (..), Cov2 (..))
import NumHask.Prelude
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)
import Downhill.Linear.Fun

-- Scalar and vector fields share a scalar type, but vector cotangents have
-- the distinct Cov2 type. Leaf splitting/joining in SplitLeaf is just id.
type Fields shapes = HListOf (SplitLeaf (V2 Int)) shapes
type Variables r shapes = HListOf (SplitLeaf (BVar r (V2 Int))) shapes
type Mixed = '[ 'SplitScalar, 'SplitVector, 'SplitScalar]

splitFields :: forall shapes r. SingI shapes => BVar r (Fields shapes) -> Variables r shapes
splitFields = splitBVarField @(TyCon1 (HListOf (SplitLeaf (V2 Int)))) (sing @shapes)

joinFields :: forall shapes r. SingI shapes => Variables r shapes -> BVar r (Fields shapes)
joinFields = joinBVarField @(TyCon1 (HListOf (SplitLeaf (V2 Int)))) (sing @shapes)

mixed :: Scalar v -> v -> Scalar v -> HListOf (SplitLeaf v) Mixed
mixed a v b = HConsOf a (HConsOf v (HConsOf b HNilOf))

input :: Fields Mixed
input = mixed 2 (V2 3 5) 7

direction :: Fields Mixed
direction = mixed 11 (V2 13 17) 19

firstField :: BVar r (Fields Mixed) -> BVar r Int
firstField x = case splitFields x of
    HConsOf a _ -> a

vectorField :: BVar r (Fields Mixed) -> BVar r (V2 Int)
vectorField x = case splitFields x of
    HConsOf _ (HConsOf v _) -> v

lastField :: BVar r (Fields Mixed) -> BVar r Int
lastField x = case splitFields x of
    HConsOf _ (HConsOf _ (HConsOf b HNilOf)) -> b

testSplitEmpty :: Assertion
testSplitEmpty = do
    value @=? 7
    derivative <*| HNilOf @=? 0
    3 |*> derivative @=? (HNilOf :: HListOf (SplitLeaf (Cov2 Int)) '[])
  where
    objective :: BVar r (Fields '[]) -> BVar r Int
    objective x = case splitFields x of
        HNilOf -> BVar.constant 7
    (value, derivative) = BVar.unDFun (BVar.ad objective) HNilOf

testJoinEmpty :: Assertion
testJoinEmpty = do
    value @=? (HNilOf :: Fields '[])
    derivative <*| 11 @=? (HNilOf :: Fields '[])
    HNilOf |*> derivative @=? 0
  where
    objective :: BVar r Int -> BVar r (Fields '[])
    objective _ = joinFields HNilOf
    (value, derivative) = BVar.unDFun (BVar.ad objective) 7

testSplitSingletonScalar :: Assertion
testSplitSingletonScalar = do
    value @=? 7
    derivative <*| HConsOf 11 HNilOf @=? 11
    13 |*> derivative @=? HConsOf 13 HNilOf
  where
    objective :: BVar r (Fields '[ 'SplitScalar]) -> BVar r Int
    objective x = case splitFields x of
        HConsOf a HNilOf -> a
    (value, derivative) = BVar.unDFun (BVar.ad objective) (HConsOf 7 HNilOf)

testSplitSingletonVector :: Assertion
testSplitSingletonVector = do
    value @=? V2 3 5
    derivative <*| HConsOf (V2 7 11) HNilOf @=? V2 7 11
    Cov2 13 17 |*> derivative @=? HConsOf (Cov2 13 17) HNilOf
  where
    objective :: BVar r (Fields '[ 'SplitVector]) -> BVar r (V2 Int)
    objective x = case splitFields x of
        HConsOf v HNilOf -> v
    (value, derivative) = BVar.unDFun (BVar.ad objective) (HConsOf (V2 3 5) HNilOf)

testSplitMixed :: Assertion
testSplitMixed = do
    firstValue @=? 2
    firstDerivative <*| direction @=? 11
    23 |*> firstDerivative @=? mixed 23 (Cov2 0 0) 0
    vectorValue @=? V2 3 5
    vectorDerivative <*| direction @=? V2 13 17
    Cov2 23 29 |*> vectorDerivative @=? mixed 0 (Cov2 23 29) 0
    lastValue @=? 7
    lastDerivative <*| direction @=? 19
    23 |*> lastDerivative @=? mixed 0 (Cov2 0 0) 23
  where
    (firstValue, firstDerivative) = BVar.unDFun (BVar.ad firstField) input
    (vectorValue, vectorDerivative) = BVar.unDFun (BVar.ad vectorField) input
    (lastValue, lastDerivative) = BVar.unDFun (BVar.ad lastField) input

generated :: BVar r Int -> Variables r Mixed
generated x = mixed (x |* 2) (BVar.constant (V2 3 5) |* x) (x |* 7)

testJoinSingletons :: Assertion
testJoinSingletons = do
    scalarValue @=? HConsOf 14 HNilOf
    scalarDerivative <*| 11 @=? HConsOf 22 HNilOf
    HConsOf 13 HNilOf |*> scalarDerivative @=? 26
    vectorValue @=? HConsOf (V2 21 35) HNilOf
    vectorDerivative <*| 11 @=? HConsOf (V2 33 55) HNilOf
    HConsOf (Cov2 13 17) HNilOf |*> vectorDerivative @=? 124
  where
    scalar :: BVar r Int -> BVar r (Fields '[ 'SplitScalar])
    scalar x = joinFields (HConsOf (x |* 2) HNilOf)
    vector :: BVar r Int -> BVar r (Fields '[ 'SplitVector])
    vector x = joinFields (HConsOf (BVar.constant (V2 3 5) |* x) HNilOf)
    (scalarValue, scalarDerivative) = BVar.unDFun (BVar.ad scalar) 7
    (vectorValue, vectorDerivative) = BVar.unDFun (BVar.ad vector) 7

testJoinMixed :: Assertion
testJoinMixed = do
    value @=? mixed 22 (V2 33 55) 77
    derivative <*| 13 @=? mixed 26 (V2 39 65) 91
    mixed 17 (Cov2 19 23) 29 |*> derivative @=? 409
  where
    objective :: BVar r Int -> BVar r (Fields Mixed)
    objective = joinFields . generated
    (value, derivative) = BVar.unDFun (BVar.ad objective) 11

testSparseRepeatedUse :: Assertion
testSparseRepeatedUse = do
    value @=? 14
    derivative <*| direction @=? 38
    23 |*> derivative @=? mixed 0 (Cov2 0 0) 46
  where
    objective :: BVar r (Fields Mixed) -> BVar r Int
    objective x = let b = lastField x in b + b
    (value, derivative) = BVar.unDFun (BVar.ad objective) input

testJoinAfterSplit :: Assertion
testJoinAfterSplit = do
    value @=? input
    derivative <*| direction @=? direction
    cotangent |*> derivative @=? cotangent
  where
    cotangent = mixed 23 (Cov2 29 31) 37
    objective :: BVar r (Fields Mixed) -> BVar r (Fields Mixed)
    objective = joinFields . splitFields
    (value, derivative) = BVar.unDFun (BVar.ad objective) input

testSplitAfterJoin :: Assertion
testSplitAfterJoin = do
    firstValue @=? 22
    firstDerivative <*| 13 @=? 26
    17 |*> firstDerivative @=? 34
    vectorValue @=? V2 33 55
    vectorDerivative <*| 13 @=? V2 39 65
    Cov2 19 23 |*> vectorDerivative @=? 172
    lastValue @=? 77
    lastDerivative <*| 13 @=? 91
    29 |*> lastDerivative @=? 203
  where
    -- These projections operate on the sparse builders produced by joining.
    (firstValue, firstDerivative) = BVar.unDFun (BVar.ad (firstField . joinFields . generated)) 11
    (vectorValue, vectorDerivative) = BVar.unDFun (BVar.ad (vectorField . joinFields . generated)) 11
    (lastValue, lastDerivative) = BVar.unDFun (BVar.ad (lastField . joinFields . generated)) 11

fieldsTests :: SpecWith ()
fieldsTests = describe "Fields" $ do
    it "splits an empty list" testSplitEmpty
    it "joins an empty list as a constant" testJoinEmpty
    it "splits a singleton scalar" testSplitSingletonScalar
    it "splits a singleton vector with a distinct dual type" testSplitSingletonVector
    it "splits each position of a heterogeneous list" testSplitMixed
    it "joins singleton scalar and vector lists" testJoinSingletons
    it "joins dependent heterogeneous fields and accumulates cotangents" testJoinMixed
    it "accumulates repeated use while leaving unused fields at zero" testSparseRepeatedUse
    it "preserves values and derivatives when joining after splitting" testJoinAfterSplit
    it "preserves component derivatives when splitting after joining" testSplitAfterJoin
