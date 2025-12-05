{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Record (recordTests)
where

import Data.Singletons (Apply, SingI (sing), TyCon1, type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.Tree.FieldWrapper (FieldWrapper (..))
import Downhill.Tree.Record (FieldGetter, IsRecord (..), Record, recordField, RecordBuilder)
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)
import Downhill.Linear.Builder (HasBuilder(..), maybeToMonoid)
import Downhill.Tree.Record (fieldBuilder, FieldGetter (FieldGetter))

data MonoLeaf (a::Type) :: () ~> Type
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

-- TODO: derive this via something
instance (Additive a, SingI sh) => HasBuilder (FieldWrapper (MonoLeaf a) sh) where
    type VecBuilder (FieldWrapper (MonoLeaf a) sh) = Maybe (Sum a)
    sumBuilder = case sing @sh of
        STuple0 -> FieldWrapper . getSum . maybeToMonoid
    identityBuilder = case sing @sh of
        STuple0 -> Just . Sum . unFieldWrapper


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

builderTests :: SpecWith ()
builderTests = it "builderTests" $ do
    let mkRecord :: Int -> Int -> FieldWrapper (Record MyRecord) (MonoLeaf Int)
        mkRecord x y = FieldWrapper (MyRecord x y)
        fs :: MyRecord (TyCon1 (FieldGetter MyRecord))
        fs = recordGetters
        mkBuilder :: FieldGetter MyRecord '() -> Int -> RecordBuilder MyRecord (MonoLeaf Int)
        mkBuilder field value = fieldBuilder field (Just (Sum value))
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
