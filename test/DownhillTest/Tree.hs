{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module DownhillTest.Tree (treeTests)
where

import Data.Singletons (Apply, SingI (sing), TyCon1, type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.BVar (BVar, DFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Linear.Fun ((|*>), (<*|))
import Downhill.Tree.BVar (BVarRepr (joinBVarField))
import Downhill.Tree.Record (IsRecord)
import Downhill.Tree.Tree (
    Tree (..),
    TreeOf,
    TreeShape (TreeShapeLeaf, TreeShapeRecord),
    adTree,
    adTreeRecord,
    fillTreeEx,
    foldTreeEx,
    mapTreeEx,
    zipTreeEx,
 )
import DownhillTest.MonoLeaf (MonoLeaf)
import GHC.Records (getField)
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)

type Leaf f = f @@ 'TreeShapeLeaf '()
type Subtree f record = f @@ 'TreeShapeRecord record

data MyRecord (f :: TreeShape () ~> Type) = MyRecord
    { fieldA :: Leaf f
    , fieldB :: Leaf f
    }
    deriving (Generic)

data MyBigRecord (f :: TreeShape () ~> Type) = MyBigRecord
    { subrA :: Subtree f MyRecord
    , subrB :: Subtree f MyRecord
    , fieldC :: Leaf f
    }
    deriving (Generic)

instance IsRecord MyRecord
deriving instance (Eq a) => Eq (MyRecord (TreeOf (MonoLeaf a)))
deriving instance (Show a) => Show (MyRecord (TreeOf (MonoLeaf a)))
instance IsRecord MyBigRecord
deriving instance (Eq a) => Eq (MyBigRecord (TreeOf (MonoLeaf a)))
deriving instance (Show a) => Show (MyBigRecord (TreeOf (MonoLeaf a)))

type MyTreeValue = MyBigRecord (TreeOf (MonoLeaf Int))

testFill1 :: Assertion
testFill1 = myTree @=? expected
  where
    myTree, expected :: MyTreeValue
    myTree = fillTreeEx (\STuple0 -> 4) (sing @('TreeShapeRecord MyBigRecord))
    expected = MyBigRecord (MyRecord 4 4) (MyRecord 4 4) 4

testExplicitOperations :: Assertion
testExplicitOperations = do
    mapTreeEx @(MonoLeaf Int) @(MonoLeaf Int) (\STuple0 n -> n + 1) treeShape x
        @=? MyBigRecord (MyRecord 2 3) (MyRecord 4 5) 6
    zipTreeEx @(MonoLeaf Int) @(MonoLeaf Int) @(MonoLeaf Int) (\STuple0 a b -> a + b) treeShape x y
        @=? MyBigRecord (MyRecord 8 10) (MyRecord 12 14) 16
    foldTreeEx @(MonoLeaf Int) @Int (\STuple0 n total -> n + total) treeShape 0 x @=? 15
  where
    treeShape = sing @('TreeShapeRecord MyBigRecord)
    x, y :: MyTreeValue
    x = MyBigRecord (MyRecord 1 2) (MyRecord 3 4) 5
    y = MyBigRecord (MyRecord 7 8) (MyRecord 9 10) 11

myFun :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => MyBigRecord (TreeOf (MonoLeaf a)) -> a
myFun x = (x.subrA.fieldA |* 3) + (x.subrA.fieldB |* 5) + (x.subrB.fieldB |* 7) + (x.fieldC |* 11)

myFunT :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => a -> MyBigRecord (TreeOf (MonoLeaf a))
myFunT x = MyBigRecord (MyRecord (x |* 3) (x |* 5)) (MyRecord (x |* 7) (x |* 11)) (x |* 17)

type NewTree = Tree (MonoLeaf Int) ('TreeShapeRecord MyBigRecord)

newTreeInstancesTest :: Assertion
newTreeInstancesTest = do
    unTree (x + y) @=? MyBigRecord (MyRecord 8 10) (MyRecord 12 14) 16
    unTree (sumBuilder (identityBuilder x)) @=? unTree x
    unTree (sumBuilder (identityBuilder x <> identityBuilder y)) @=? unTree (x + y)
  where
    x, y :: NewTree
    x = Tree (MyBigRecord (MyRecord 1 2) (MyRecord 3 4) 5)
    y = Tree (MyBigRecord (MyRecord 7 8) (MyRecord 9 10) 11)

joinNewTree :: Tree (MonoLeaf (BVar r Int)) ('TreeShapeRecord MyBigRecord) -> BVar r NewTree
joinNewTree = joinBVarField @(TyCon1 (Tree (MonoLeaf Int))) sing

newTreeSplitTest :: Assertion
newTreeSplitTest = do
    value @=? 3 * 23 + 5 * 29 + 7 * 37 + 11 * 41
    unTree (13 |*> derivative)
        @=? MyBigRecord (MyRecord (13 * 3) (13 * 5)) (MyRecord 0 (13 * 7)) (13 * 11)
    derivative <*| Tree (MyBigRecord (MyRecord 17 19) (MyRecord 43 47) 53)
        @=? 3 * 17 + 5 * 19 + 7 * 47 + 11 * 53
  where
    objective :: DFun NewTree Int
    objective = adTreeRecord myFun
    input = Tree (MyBigRecord (MyRecord 23 29) (MyRecord 31 37) 41)
    (value, derivative) = BVar.unDFun objective input

newLeafTreeAdTest :: Assertion
newLeafTreeAdTest = do
    value @=? 49
    unTree (13 |*> derivative) @=? 182
    derivative <*| Tree 11 @=? 154
  where
    objective :: DFun (Tree (MonoLeaf Int) ('TreeShapeLeaf '())) Int
    objective = adTree (\(Tree x) -> x * x)
    (value, derivative) = BVar.unDFun objective (Tree 7)

newTreeJoinTest :: Assertion
newTreeJoinTest = do
    unTree value
        @=? MyBigRecord (MyRecord (13 * 3) (13 * 5)) (MyRecord (13 * 7) (13 * 11)) (13 * 17)
    unTree (derivative <*| 41)
        @=? MyBigRecord (MyRecord (41 * 3) (41 * 5)) (MyRecord (41 * 7) (41 * 11)) (41 * 17)
    Tree (MyBigRecord (MyRecord 19 23) (MyRecord 29 31) 37) |*> derivative
        @=? 3 * 19 + 5 * 23 + 7 * 29 + 11 * 31 + 17 * 37
  where
    objective :: BVar r Int -> BVar r NewTree
    objective = joinNewTree . Tree . myFunT
    (value, derivative) = BVar.unDFun (BVar.ad objective) 13

treeTests :: SpecWith ()
treeTests = describe "Tree" $ do
    it "fill" testFill1
    it "supports explicit structural operations" testExplicitOperations
    it "supports algebra and builders through Tree" newTreeInstancesTest
    it "differentiates a leaf-root Tree" newLeafTreeAdTest
    it "splits Tree variables" newTreeSplitTest
    it "joins Tree variables" newTreeJoinTest
    return ()
