{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Tree (treeTests)
where

import Data.Singletons (Apply, SingI (sing), TyCon1, type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Tree.FieldWrapper (DenseFieldWrapper (..), FieldWrapper (..))
import Downhill.Tree.Record (
    FieldGetter,
    IsRecord (..),
    Record,
    RecordBuilder,
    ShR (..),
    fieldBuilder,
    recordField,
 )
import Downhill.Tree.Tree (TreeOf, TreeShape (TreeShapeLeaf, TreeShapeRecord), fillTree)
import DownhillTest.MonoLeaf (Field, MonoLeaf)
import NumHask.Prelude
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, assert, (@=?))
import Test.Hspec (SpecWith, describe, it)

type Leaf f = f @@ 'TreeShapeLeaf '()
type Subtree f record = f @@ 'TreeShapeRecord record

data MyRecord (f :: TreeShape () ~> Type) = MyRecord
    { fieldA :: Leaf f
    , fieldB :: Leaf f
    }
    deriving (Generic)

data MyBigRecord (f :: TreeShape () ~> Type) = MyBygRecord
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

type MyTree = FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord)

deriving instance Show MyTree

testFill1 :: Assertion
testFill1 = myTree @=? expected
  where
    myTree, expected :: MyTree
    myTree = fillTree 4
    expected = FieldWrapper (MyBygRecord (MyRecord 4 4) (MyRecord 4 4) 4)

testEq :: Assertion
testEq = do
    assert (x == x)
    assert (x /= y)
  where
    x, y :: MyTree
    x = FieldWrapper (MyBygRecord (MyRecord 4 4) (MyRecord 4 4) 4)
    y = FieldWrapper (MyBygRecord (MyRecord 4 4) (MyRecord 4 3) 4)

treeTests :: SpecWith ()
treeTests = describe "Record" $ do
    it "fill" testFill1
    it "eq" testEq
    return ()
