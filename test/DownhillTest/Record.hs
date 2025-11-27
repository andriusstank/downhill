{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Record (recordTests)
where

import Data.Singletons (Apply, SingI (sing), TyCon1, type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.Tree.FieldWrapper (FieldWrapper (..))
import Downhill.Tree.Record (FieldGetter, IsRecord (..), Record, recordField)
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)

data MonoLeaf a :: () ~> Type
type instance Apply (MonoLeaf a) '() = a

instance (SingI sh, Additive a) => Additive (FieldWrapper (MonoLeaf a) sh) where
    zero = case sing @sh of
        STuple0 -> FieldWrapper zero
    FieldWrapper x + FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x + y)

instance (SingI sh, Multiplicative a) => Multiplicative (FieldWrapper (MonoLeaf a) sh) where
    one = case sing @sh of
        STuple0 -> FieldWrapper one
    FieldWrapper x * FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x * y)

type Field f = f @@ '()

data MyRecord (f :: () ~> Type) = MyRecord
    { fieldA :: Field f
    , fieldB :: Field f
    }
    deriving (Generic)

instance IsRecord MyRecord
deriving instance (Eq a) => Eq (MyRecord (MonoLeaf a))
deriving instance (Show a) => Show (MyRecord (MonoLeaf a))

testFill1 :: Assertion
testFill1 = fillRecordEx f @=? (MyRecord 3 3 :: MyRecord (MonoLeaf Int))
  where
    f :: STuple0 sh -> MonoLeaf Int @@ sh
    f STuple0 = 3

testFill2 :: Assertion
testFill2 = fillRecordEx f @=? (MyRecord 1 1 :: MyRecord (MonoLeaf Int))
  where
    f :: STuple0 sh -> MonoLeaf Int @@ sh
    f STuple0 = one

deriving instance Show (FieldWrapper (Record MyRecord) (MonoLeaf Int))
deriving instance Eq (FieldWrapper (Record MyRecord) (MonoLeaf Int))

testPlus :: Assertion
testPlus = FieldWrapper (MyRecord 8 10) @=? (x + y)
  where
    x, y :: FieldWrapper (Record MyRecord) (MonoLeaf Int)
    x = FieldWrapper (MyRecord 3 4)
    y = FieldWrapper (MyRecord 5 6)

testField :: Assertion
testField =
    do
        recordField getters.fieldA x @=? 3
        recordField getters.fieldB x @=? 4
  where
    x :: MyRecord (MonoLeaf Int)
    x = MyRecord 3 4

    getters :: MyRecord (TyCon1 (FieldGetter MyRecord))
    getters = recordGetters

recordTests :: SpecWith ()
recordTests = describe "Record" $ do
    it "fill" (testFill1 >> testFill2)
    it "plus" testPlus
    it "getter" testField
