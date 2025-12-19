{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Record (recordTests)
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
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)
import DownhillTest.MonoLeaf (Field, MonoLeaf)


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

deriving instance Show (FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord))
deriving instance Eq (FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord))

testPlus :: Assertion
testPlus = FieldWrapper (MyRecord 8 10) @=? (x + y)
  where
    x, y :: FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord)
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
    getters = recordGettersEx

builderTests :: SpecWith ()
builderTests = it "builderTests" $ do
    let mkRecord :: Int -> Int -> FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord)
        mkRecord x y = FieldWrapper (MyRecord x y)
        fs :: MyRecord (TyCon1 (FieldGetter MyRecord))
        fs = recordGettersEx
        mkBuilder :: FieldGetter MyRecord '() -> Int -> RecordBuilder ('ShR MyRecord) (MonoLeaf Int)
        mkBuilder field value = fieldBuilder field (Just (Sum (FieldWrapper value)))
    sumBuilder mempty @=? mkRecord 0 0
    sumBuilder (identityBuilder (mkRecord 3 4)) @=? mkRecord 3 4
    sumBuilder (mkBuilder fs.fieldA 5) @=? mkRecord 5 0
    sumBuilder (mkBuilder fs.fieldA 5 <> mkBuilder fs.fieldA 6) @=? mkRecord 11 0
    sumBuilder (identityBuilder (mkRecord 7 8) <> mkBuilder fs.fieldA 5 <> mkBuilder fs.fieldA 6) @=? mkRecord 18 8
    sumBuilder (mkBuilder fs.fieldB 9) @=? mkRecord 0 9
    sumBuilder (mkBuilder fs.fieldB 9 <> mkBuilder fs.fieldA 10) @=? mkRecord 10 9
    sumBuilder (mkBuilder fs.fieldB 9 <> mkBuilder fs.fieldA 10 <> identityBuilder (mkRecord 10 20)) @=? mkRecord 20 29

recordTests :: SpecWith ()
recordTests = describe "Record" $ do
    it "fill" (testFill1 >> testFill2)
    it "plus" testPlus
    it "getter" testField
    builderTests
