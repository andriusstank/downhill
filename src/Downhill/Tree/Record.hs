{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Record (
    IsRecord (..),
    Record,
    fillRecord,
    mapRecord,
    zipRecord,
)
where

import Data.Singletons (Apply, Proxy, Sing, SingI (sing), type (@@), type (~>), withSingI)
import NumHask.Prelude
import Downhill.Tree.FieldWrapper (FieldWrapper (..), AdditiveField, SubtractiveField, MultiplicativeField, FieldScalar, MultiplicativeActionField)

data ProxyField :: leafShape -> Type
type instance Apply ProxyField sh = Proxy sh

type IsRecord :: forall {fieldType}. ((fieldType ~> Type) -> Type) -> Constraint
class IsRecord (record :: (fieldType ~> Type) -> Type) where
    fillRecordEx :: (forall sh. Sing sh -> f @@ sh) -> record f
    default fillRecordEx ::
        forall f.
        ( Generic (record f)
        , GFillRecord (Rep (record ProxyField)) f (Rep (record f))
        ) =>
        (forall sh. Sing sh -> f @@ sh) ->
        record f
    fillRecordEx f = to $ fillGRecord @(Rep (record ProxyField)) @f f

    mapRecordEx :: (forall sh. Sing sh -> a @@ sh -> b @@ sh) -> record a -> record b
    default mapRecordEx ::
        forall a b.
        ( Generic (record a)
        , Generic (record b)
        , GMapRecord
            (Rep (record ProxyField))
            a
            b
            (Rep (record a))
            (Rep (record b))
        ) =>
        (forall sh. Sing sh -> a @@ sh -> b @@ sh) ->
        record a ->
        record b
    mapRecordEx f = to . mapGRecord @(Rep (record ProxyField)) @a @b f . from

    zipRecordEx :: (forall sh. Sing sh -> a @@ sh -> b @@ sh -> c @@ sh) -> record a -> record b -> record c
    default zipRecordEx ::
        forall a b c.
        ( Generic (record a)
        , Generic (record b)
        , Generic (record c)
        , GZipRecord
            (Rep (record ProxyField))
            a
            b
            c
            (Rep (record a))
            (Rep (record b))
            (Rep (record c))
        ) =>
        (forall sh. Sing sh -> a @@ sh -> b @@ sh -> c @@ sh) ->
        record a ->
        record b ->
        record c
    zipRecordEx f x y = to (zipGRecord @(Rep (record ProxyField)) @a @b @c f (from x) (from y))

{-
fillRecordI :: forall record f. (IsRecord record) => (forall sh. (SingI sh) => FieldWrapper f sh) -> record f
fillRecordI f = fillRecord g
  where
    g :: forall sh. Sing sh -> f @@ sh
    g sh = unFieldWrapper (withSingI sh (f @sh))

mapRecordI :: forall record a b. (IsRecord record) => (forall sh. (SingI sh) => FieldWrapper a sh -> FieldWrapper b sh) -> record a -> record b
mapRecordI f = mapRecord g
  where
    g :: forall sh. Sing sh -> a @@ sh -> b @@ sh
    g sh = unFieldWrapper . withSingI sh (f @sh) . FieldWrapper
-}

-- type RecField :: ((fieldType ~> Type) -> Type) ~> Type
-- data RecField record
-- type

{-
    -- TODO: rename to zipWith

    foldRecord :: (Monoid m) => FoldFieldFun tt m -> record tt -> m
    default foldRecord ::
        ( Monoid m
        , Generic (record tt)
        , GFoldTree (Rep (record ProxyField)) tt (Rep (record tt)) m
        ) =>
        FoldFieldFun tt m ->
        record tt ->
        m
    foldRecord = genericRecordFold

    sequenceRecord :: forall f tt. (Applicative f) => record (EffField f tt) -> f (record tt)
    default sequenceRecord ::
        forall f tt.
        ( Applicative f
        , Generic (record tt)
        , Generic (record (EffField f tt))
        , GSeqRecord
            f
            (Rep (record (EffField f tt)))
            (Rep (record tt))
        ) =>
        record (EffField f tt) ->
        f (record tt)
    sequenceRecord = genericRecordSeq

    traverseRecord :: forall ttA ttB f. (Applicative f) => TraverseFieldFun f ttA ttB -> record ttA -> f (record ttB)
    default traverseRecord ::
        forall ttA ttB f.
        ( Applicative f
        , Generic (record ttA)
        , Generic (record ttB)
        , GTraverseRecord
            (Rep (record ProxyField))
            f
            ttA
            ttB
            (Rep (record ttA))
            (Rep (record ttB))
        ) =>
        TraverseFieldFun f ttA ttB ->
        record ttA ->
        f (record ttB)
    traverseRecord = genericRecordTraverse

    recordGetters :: record (FieldGetter record)
    default recordGetters ::
        ( Generic (record (FieldGetter record))
        , GettersSubtree '[] (Rep (record (FieldGetter record)))
        ) =>
        record (FieldGetter record)
    recordGetters = to (getGetters @'[] @(Rep (record (FieldGetter record))))
-}

data Record (r :: (fieldType ~> Type) -> Type) :: (fieldType ~> Type) ~> Type
type instance Apply (Record r) field = r field

fillRecord ::
    forall record f.
    (IsRecord record) =>
    (forall sh. (SingI sh) => FieldWrapper f sh) ->
    FieldWrapper (Record record) f
fillRecord f = FieldWrapper (fillRecordEx g)
  where
    g :: forall sh. Sing sh -> f @@ sh
    g sh = unFieldWrapper (withSingI sh (f @sh))

mapRecord ::
    forall record a b.
    (IsRecord record) =>
    (forall sh. (SingI sh) => FieldWrapper a sh -> FieldWrapper b sh) ->
    FieldWrapper (Record record) a ->
    FieldWrapper (Record record) b
mapRecord f (FieldWrapper x) = FieldWrapper (mapRecordEx g x)
  where
    g :: forall sh. Sing sh -> a @@ sh -> b @@ sh
    g sh = unFieldWrapper . withSingI sh (f @sh) . FieldWrapper

zipRecord ::
    forall record a b c.
    (IsRecord record) =>
    (forall sh. (SingI sh) => FieldWrapper a sh -> FieldWrapper b sh -> FieldWrapper c sh) ->
    FieldWrapper (Record record) a ->
    FieldWrapper (Record record) b ->
    FieldWrapper (Record record) c
zipRecord f (FieldWrapper x) (FieldWrapper y) = FieldWrapper (zipRecordEx g x y)
  where
    g :: forall sh. Sing sh -> a @@ sh -> b @@ sh -> c @@ sh
    g sh x' y' = unFieldWrapper (withSingI sh (f @sh (FieldWrapper x') (FieldWrapper y')))

instance (IsRecord record, AdditiveField field) => Additive (FieldWrapper (Record record) field) where
    zero = fillRecord zero
    (+) = zipRecord (+)

instance (IsRecord record, SubtractiveField field) => Subtractive (FieldWrapper (Record record) field) where
    negate = mapRecord negate
    (-) = zipRecord (-)

instance (IsRecord record, MultiplicativeField field) => Multiplicative (FieldWrapper (Record record) field) where
    one = fillRecord one
    (*) = zipRecord (*)

instance (IsRecord record, MultiplicativeActionField field) => MultiplicativeAction (FieldWrapper (Record record) field) where
    type Scalar (FieldWrapper (Record record) field) = FieldScalar field
    v |* a = mapRecord (|*a) v

-------------- Fill --------------

class GFillRecord metaTT tt (grecord :: Type -> Type) where
    fillGRecord :: (forall sh. Sing sh -> tt @@ sh) -> grecord x
instance (GFillRecord metaTT tt f) => GFillRecord (M1 i c metaTT) tt (M1 i c f) where
    fillGRecord f = M1 (fillGRecord @metaTT @tt f)
instance (SingI sh, f ~ tt @@ sh) => GFillRecord (K1 R (Proxy sh)) tt (K1 R f) where
    fillGRecord f = K1 (f (sing @sh))
instance (GFillRecord mf tt f, GFillRecord mg tt g) => GFillRecord (mf :*: mg) tt (f :*: g) where
    fillGRecord f = fillGRecord @mf @tt f :*: fillGRecord @mg @tt f
instance GFillRecord U1 f U1 where
    fillGRecord _ = U1

-------------- Map --------------

class GMapRecord metaTT ttA ttB (gtree1 :: Type -> Type) (gtree2 :: Type -> Type) where
    mapGRecord :: (forall sh. Sing sh -> ttA @@ sh -> ttB @@ sh) -> gtree1 x -> gtree2 x
instance GMapRecord U1 ttA ttB U1 U1 where
    mapGRecord _ U1 = U1
instance (GMapRecord metaTT ttA ttB f1 f2) => GMapRecord (M1 i c metaTT) ttA ttB (M1 i c f1) (M1 i c f2) where
    mapGRecord f (M1 x) = M1 (mapGRecord @metaTT @ttA @ttB f x)
instance (SingI sh, f1 ~ ttA @@ sh, f2 ~ ttB @@ sh) => GMapRecord (K1 R (Proxy sh)) ttA ttB (K1 R f1) (K1 R f2) where
    mapGRecord f (K1 x) = K1 (f (sing @sh) x)

instance (GMapRecord mf tt1 tt2 f1 f2, GMapRecord mg tt1 tt2 g1 g2) => GMapRecord (mf :*: mg) tt1 tt2 (f1 :*: g1) (f2 :*: g2) where
    mapGRecord f (x :*: y) = mapGRecord @mf @tt1 @tt2 f x :*: mapGRecord @mg @tt1 @tt2 f y

-------------- Zip --------------

class GZipRecord metaTT a b c (gtree1 :: Type -> Type) (gtree2 :: Type -> Type) (gtree3 :: Type -> Type) where
    zipGRecord :: (forall sh. Sing sh -> a @@ sh -> b @@ sh -> c @@ sh) -> gtree1 x -> gtree2 x -> gtree3 x

instance GZipRecord U1 ttA ttB ttC U1 U1 U1 where
    zipGRecord _ U1 U1 = U1

instance (GZipRecord metaTT ttA ttB ttC fA fB fC) => GZipRecord (M1 i c metaTT) ttA ttB ttC (M1 i c fA) (M1 i c fB) (M1 iz c fC) where
    zipGRecord f (M1 x) (M1 y) = M1 (zipGRecord @metaTT @ttA @ttB @ttC f x y)

instance
    ( SingI sh
    , fA ~ ttA @@ sh
    , fB ~ ttB @@ sh
    , fC ~ ttC @@ sh
    ) =>
    GZipRecord
        (K1 R (Proxy sh))
        ttA
        ttB
        ttC
        (K1 R fA)
        (K1 R fB)
        (K1 R fC)
    where
    zipGRecord f (K1 x) (K1 y) = K1 (f (sing @sh) x y)

instance (GZipRecord mf tt1 tt2 tt3 f1 f2 f3, GZipRecord mg tt1 tt2 tt3 g1 g2 g3) => GZipRecord (mf :*: mg) tt1 tt2 tt3 (f1 :*: g1) (f2 :*: g2) (f3 :*: g3) where
    zipGRecord f (x1 :*: x2) (y1 :*: y2) = zipGRecord @mf @tt1 @tt2 @tt3 f x1 y1 :*: zipGRecord @mg @tt1 @tt2 @tt3 f x2 y2
