{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.RegularRecord.BVar (bvarTests) where

import Data.Singletons (SingI (..), TyCon1)
import Downhill.BVar (BVar)
import Downhill.BVar qualified as BVar
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.RegularRecord (RecordTree (..), RegularTreeShape (..))
import DownhillTest.VSpace.V2 (V2 (..), Cov2 (..))
import DownhillTest.RegularRecord.Fixtures
import NumHask.Prelude
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)
import Downhill.Linear.Fun ((<*|), (|*>))

splitTestRecord :: BVar r SplitRecordTree -> SplitRecordB (SplitLeaf (BVar r Int))
splitTestRecord = unRecordTree . splitBVarField @(TyCon1 (RecordTree (SplitLeaf Int))) sing

testSplitRecordTree :: Assertion
testSplitRecordTree = do
    -- Exercise every field, including both named and positional subrecords.
    -- Reusing a tests accumulation of contributions to the same leaf.
    value @=? 56
    splitRecordTreeValues (13 |*> derivative) @=? (91, 26, 91, 65, 13)
    derivative <*| splitRecordTree 17 19 23 29 31 @=? 494
  where
    input = splitRecordTree 2 3 5 7 11
    objective :: BVar r SplitRecordTree -> BVar r Int
    objective x = case splitTestRecord x of
        SplitRecordB (SplitRecordA a b) (SplitTupleRecord c d) e ->
            a * a + a * b + c * d + e
    (value, derivative) = BVar.unDFun (BVar.ad objective) input

testSplitRecordTreeSparse :: Assertion
testSplitRecordTreeSparse = do
    -- Only tail fields contribute; unused siblings must receive zero gradients.
    value @=? 17
    splitRecordTreeValues (3 |*> derivative) @=? (0, 3, 0, 6, 0)
    derivative <*| splitRecordTree 17 19 23 29 31 @=? 77
  where
    objective :: BVar r SplitRecordTree -> BVar r Int
    objective x = case splitTestRecord x of
        SplitRecordB (SplitRecordA _ b) (SplitTupleRecord _ d) _ -> b + d + d
    (value, derivative) = BVar.unDFun (BVar.ad objective) (splitRecordTree 2 3 5 7 11)

-- Call the instance methods directly so both leaf and subtree dispatch are tested.
type LeafTree sh = RecordTree (SplitLeaf (V2 Int)) ('RegularTreeShapeLeaf sh)

splitLeafTree :: forall sh r. SingI sh => BVar r (LeafTree sh) -> RecordTree (SplitLeaf (BVar r (V2 Int))) ('RegularTreeShapeLeaf sh)
splitLeafTree = splitBVarField @(TyCon1 (RecordTree (SplitLeaf (V2 Int)))) sing

joinLeafTree :: forall sh r. SingI sh => RecordTree (SplitLeaf (BVar r (V2 Int))) ('RegularTreeShapeLeaf sh) -> BVar r (LeafTree sh)
joinLeafTree = joinBVarField @(TyCon1 (RecordTree (SplitLeaf (V2 Int)))) sing

testSplitScalarLeaf :: Assertion
testSplitScalarLeaf = do
    value @=? 7
    derivative <*| RecordTree 11 @=? 11
    unRecordTree (13 |*> derivative) @=? 13
  where
    objective :: BVar r (LeafTree 'SplitScalar) -> BVar r Int
    objective = unRecordTree . splitLeafTree
    (value, derivative) = BVar.unDFun (BVar.ad objective) (RecordTree 7)

testSplitVectorLeaf :: Assertion
testSplitVectorLeaf = do
    value @=? V2 3 5
    derivative <*| RecordTree (V2 7 11) @=? V2 7 11
    unRecordTree (Cov2 13 17 |*> derivative) @=? Cov2 13 17
  where
    objective :: BVar r (LeafTree 'SplitVector) -> BVar r (V2 Int)
    objective = unRecordTree . splitLeafTree
    (value, derivative) = BVar.unDFun (BVar.ad objective) (RecordTree (V2 3 5))

testJoinScalarLeaf :: Assertion
testJoinScalarLeaf = do
    unRecordTree value @=? 14
    unRecordTree (derivative <*| 11) @=? 22
    RecordTree 13 |*> derivative @=? 26
  where
    objective :: BVar r Int -> BVar r (LeafTree 'SplitScalar)
    objective x = joinLeafTree (RecordTree (x |* 2))
    (value, derivative) = BVar.unDFun (BVar.ad objective) 7

testJoinVectorLeaf :: Assertion
testJoinVectorLeaf = do
    unRecordTree value @=? V2 21 35
    unRecordTree (derivative <*| 11) @=? V2 33 55
    RecordTree (Cov2 13 17) |*> derivative @=? 124
  where
    objective :: BVar r Int -> BVar r (LeafTree 'SplitVector)
    objective x = joinLeafTree (RecordTree (BVar.constant (V2 3 5) |* x))
    (value, derivative) = BVar.unDFun (BVar.ad objective) 7

joinTestRecord :: SplitRecordB (SplitLeaf (BVar r Int)) -> BVar r SplitRecordTree
joinTestRecord = joinBVarField @(TyCon1 (RecordTree (SplitLeaf Int))) sing . RecordTree

generatedRecord :: BVar r Int -> SplitRecordB (SplitLeaf (BVar r Int))
generatedRecord x = SplitRecordB
    (SplitRecordA (x |* 2) (x |* 3))
    (SplitTupleRecord (x |* 5) (x |* 7))
    (BVar.constant 11)

testJoinRecordTree :: Assertion
testJoinRecordTree = do
    splitRecordTreeValues value @=? (26, 39, 65, 91, 11)
    splitRecordTreeValues (derivative <*| 17) @=? (34, 51, 85, 119, 0)
    splitRecordTree 19 23 29 31 37 |*> derivative @=? 469
  where
    objective :: BVar r Int -> BVar r SplitRecordTree
    objective = joinTestRecord . generatedRecord
    (value, derivative) = BVar.unDFun (BVar.ad objective) 13

testRecordJoinAfterSplit :: Assertion
testRecordJoinAfterSplit = do
    splitRecordTreeValues value @=? (2, 3, 5, 7, 11)
    splitRecordTreeValues (derivative <*| direction) @=? (13, 17, 19, 23, 29)
    splitRecordTreeValues (cotangent |*> derivative) @=? (31, 37, 41, 43, 47)
  where
    objective :: BVar r SplitRecordTree -> BVar r SplitRecordTree
    objective = joinTestRecord . splitTestRecord
    direction = splitRecordTree 13 17 19 23 29
    cotangent = splitRecordTree 31 37 41 43 47
    (value, derivative) = BVar.unDFun (BVar.ad objective) (splitRecordTree 2 3 5 7 11)

testRecordSplitAfterJoin :: Assertion
testRecordSplitAfterJoin = do
    value @=? 232
    derivative <*| 17 @=? 289
    19 |*> derivative @=? 323
  where
    -- Sparse reverse contributions cross both named and positional subrecords;
    -- the repeated tail field must accumulate and the constant contributes zero.
    objective :: BVar r Int -> BVar r Int
    objective x = case splitTestRecord (joinTestRecord (generatedRecord x)) of
        SplitRecordB (SplitRecordA _ b) (SplitTupleRecord _ d) e -> b + d + d + e
    (value, derivative) = BVar.unDFun (BVar.ad objective) 13

bvarTests :: SpecWith ()
bvarTests = describe "BVar" $ do
    it "splits record variables preserving values and forward/reverse derivatives" testSplitRecordTree
    it "splits record variables with sparse and repeated field use" testSplitRecordTreeSparse

    it "splits scalar leaf trees" testSplitScalarLeaf
    it "splits vector leaf trees with distinct cotangents" testSplitVectorLeaf
    it "joins scalar leaf trees" testJoinScalarLeaf
    it "joins vector leaf trees with distinct cotangents" testJoinVectorLeaf
    it "joins nested records with shared variables and constant fields" testJoinRecordTree
    it "preserves values and derivatives when joining a split record" testRecordJoinAfterSplit
    it "propagates sparse repeated contributions when splitting a joined record" testRecordSplitAfterJoin
