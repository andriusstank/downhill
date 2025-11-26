{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.FieldWrapper (
    FieldWrapper (..),
    fillRecordI, mapRecordI, zipRecordI
)
where

import Data.Singletons (Sing, SingI, TyCon1, withSingI, type (@@))
import Downhill.Tree.Record (IsRecord (..))
import NumHask.Prelude

newtype FieldWrapper field sh = FieldWrapper {unFieldWrapper :: field @@ sh}

-- instance Eq (FieldWrapper (TyCon f) sh) where

class (forall sh. (SingI sh) => Additive (FieldWrapper f sh)) => AdditiveField f
instance (forall sh. (SingI sh) => Additive (FieldWrapper f sh)) => AdditiveField f

class (forall sh. (SingI sh) => Subtractive (FieldWrapper f sh)) => SubtractiveField f
instance (forall sh. (SingI sh) => Subtractive (FieldWrapper f sh)) => SubtractiveField f

-- >>> :type fillRecord
-- fillRecord
--   :: forall {fieldType} (record :: (fieldType ~> *) -> *)
--             (f :: fieldType ~> *).
--      IsRecord record =>
--      (forall (sh :: fieldType). Sing sh -> f @@ sh) -> record f

fillRecordI :: forall record f. (IsRecord record) => (forall sh. (SingI sh) => FieldWrapper f sh) -> FieldWrapper (TyCon1 record) f
fillRecordI f = FieldWrapper (fillRecord g)
  where
    g :: forall sh. Sing sh -> f @@ sh
    g sh = unFieldWrapper (withSingI sh (f @sh))

mapRecordI ::
    forall record a b.
    (IsRecord record) =>
    (forall sh. (SingI sh) => FieldWrapper a sh -> FieldWrapper b sh) ->
    FieldWrapper (TyCon1 record) a ->
    FieldWrapper (TyCon1 record) b
mapRecordI f (FieldWrapper x) = FieldWrapper (mapRecord g x)
  where
    g :: forall sh. Sing sh -> a @@ sh -> b @@ sh
    g sh = unFieldWrapper . withSingI sh (f @sh) . FieldWrapper

zipRecordI ::
    forall record a b c.
    (IsRecord record) =>
    (forall sh. (SingI sh) => FieldWrapper a sh -> FieldWrapper b sh -> FieldWrapper c sh) ->
    FieldWrapper (TyCon1 record) a ->
    FieldWrapper (TyCon1 record) b ->
    FieldWrapper (TyCon1 record) c
zipRecordI f (FieldWrapper x) (FieldWrapper y) = FieldWrapper (zipRecord g x y)
  where
    g :: forall sh. Sing sh -> a @@ sh -> b @@ sh -> c @@ sh
    g sh x' y' = unFieldWrapper (withSingI sh (f @sh (FieldWrapper x') (FieldWrapper y')))

instance (IsRecord record, AdditiveField field) => Additive (FieldWrapper (TyCon1 record) field) where
    zero = fillRecordI zero
    (+) = zipRecordI (+)

instance (IsRecord record, SubtractiveField field) => Subtractive (FieldWrapper (TyCon1 record) field) where
    negate = mapRecordI negate
    (-) = zipRecordI (-)
