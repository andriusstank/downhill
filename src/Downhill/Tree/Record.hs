{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Record (
    IsRecord (..),
    Record,
    fillRecord,
    mapRecord,
    zipRecord,
    fillRecordA,
    FieldGetter (..),
    recordField,
    fieldView,
    RecordBuilder,
    fieldBuilder,
)
where

import Control.Monad.ST.Strict (ST, runST)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.STRef.Strict (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Singletons (Apply, Proxy, Sing, SingI (sing), TyCon1, withSingI, type (@@), type (~>))
import Downhill.Linear.Builder (HasBuilder (..), maybeToMonoid)
import Downhill.Tree.FieldWrapper (AdditiveField, FieldScalar, FieldWrapper (..), MultiplicativeActionField, MultiplicativeField, SubtractiveField)
import NumHask.Prelude

data ProxyField :: leafShape -> Type
type instance Apply ProxyField sh = Proxy sh

data FieldGetter record sh = FieldGetter {sh :: Sing sh, get :: forall f. record f -> f @@ sh}

recordField :: FieldGetter record sh -> record f -> f @@ sh
recordField (FieldGetter _ f) = f

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
    fillRecordEx = to . fillGRecord @(Rep (record ProxyField)) @f

    fillRecordExA :: (Applicative g) => (forall sh. Sing sh -> g (f @@ sh)) -> g (record f)
    default fillRecordExA ::
        forall g f.
        (Applicative g) =>
        ( Generic (record f)
        , GFillRecord (Rep (record ProxyField)) f (Rep (record f))
        ) =>
        (forall sh. Sing sh -> g (f @@ sh)) ->
        g (record f)
    fillRecordExA = fmap to . fillGRecordA @(Rep (record ProxyField)) @f

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

    mapRecordExA :: (Applicative f) => (forall sh. Sing sh -> a @@ sh -> f (b @@ sh)) -> record a -> f (record b)
    default mapRecordExA ::
        forall f a b.
        (Applicative f) =>
        ( Generic (record a)
        , Generic (record b)
        , GMapRecord
            (Rep (record ProxyField))
            a
            b
            (Rep (record a))
            (Rep (record b))
        ) =>
        (forall sh. Sing sh -> a @@ sh -> f (b @@ sh)) ->
        record a ->
        f (record b)
    mapRecordExA f = fmap to . traverseGRecord @(Rep (record ProxyField)) @a @b f . from

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

    recordGetters :: record (TyCon1 (FieldGetter record))
    default recordGetters ::
        ( Generic (record (TyCon1 (FieldGetter record)))
        , GettersSubtree '[] (Rep (record (TyCon1 (FieldGetter record))))
        ) =>
        record (TyCon1 (FieldGetter record))
    recordGetters = to (getGetters @'[] @(Rep (record (TyCon1 (FieldGetter record)))))

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

fillRecordA ::
    forall record f g.
    (IsRecord record, Applicative g) =>
    (forall sh. (SingI sh) => g (FieldWrapper f sh)) ->
    g (FieldWrapper (Record record) f)
fillRecordA f = FieldWrapper <$> fillRecordExA g
  where
    g :: forall sh. Sing sh -> g (f @@ sh)
    g sh = unFieldWrapper <$> withSingI sh (f @sh)

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

mapRecordA ::
    forall f record a b.
    (IsRecord record, Applicative f) =>
    (forall sh. (SingI sh) => FieldWrapper a sh -> f (FieldWrapper b sh)) ->
    FieldWrapper (Record record) a ->
    f (FieldWrapper (Record record) b)
mapRecordA f (FieldWrapper x) = FieldWrapper <$> mapRecordExA g x
  where
    g :: forall sh. Sing sh -> a @@ sh -> f (b @@ sh)
    g sh = fmap unFieldWrapper . withSingI sh (f @sh) . FieldWrapper

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

fieldView ::
    (IsRecord record) =>
    ((SingI sh) => FieldWrapper a sh -> r) ->
    FieldGetter record sh ->
    FieldWrapper (Record record) a ->
    r
fieldView f (FieldGetter sh field) (FieldWrapper record) = withSingI sh $ f (FieldWrapper (field record))

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
    v |* a = mapRecord (|* a) v

data FieldBuilder record field = forall sh. FieldBuilder (FieldGetter record sh) (VecBuilder (FieldWrapper field sh))

fieldBuilder :: FieldGetter record sh -> VecBuilder (FieldWrapper field sh) -> VecBuilder (FieldWrapper (Record record) field)
fieldBuilder getter builder = RecordBuilder Nothing (DList.singleton (FieldBuilder getter builder))

data FieldAccum s (field :: sh ~> Type) :: sh ~> Type
type instance Apply (FieldAccum s field) sh = STRef s (VecBuilder (FieldWrapper field sh))

data RecordBuilder record field = RecordBuilder
    { densePart :: Maybe (Sum (FieldWrapper (Record record) field))
    , fields :: DList (FieldBuilder record field)
    }

instance (IsRecord record, AdditiveField field) => Semigroup (RecordBuilder record field) where
    RecordBuilder x1 y1 <> RecordBuilder x2 y2 = RecordBuilder (x1 <> x2) (y1 <> y2)

instance (IsRecord record, AdditiveField field) => Monoid (RecordBuilder record field) where
    mempty = RecordBuilder mempty mempty

sumDensePart ::
    (IsRecord record, forall sh. (SingI sh) => HasBuilder (FieldWrapper field sh)) =>
    Maybe (Sum (FieldWrapper (Record record) field)) -> FieldWrapper (Record record) field
sumDensePart = getSum . maybeToMonoid

instance
    ( AdditiveField field
    , IsRecord record
    , forall sh. (SingI sh) => HasBuilder (FieldWrapper field sh)
    ) =>
    HasBuilder (FieldWrapper (Record record) field)
    where
    type
        VecBuilder (FieldWrapper (Record record) field) =
            RecordBuilder record field
    identityBuilder x = RecordBuilder (Just (Sum x)) mempty
    sumBuilder rb = case toList rb.fields of
        [] -> sumDensePart rb.densePart -- don't descend into fields, keep identityBuilder quick
        fields -> runST go
          where
            initField :: (SingI sh) => FieldWrapper field sh -> ST s (FieldWrapper (FieldAccum s field) sh)
            initField x = FieldWrapper <$> newSTRef (identityBuilder x)
            bumpField :: (SingI sh) => VecBuilder (FieldWrapper field sh) -> FieldWrapper (FieldAccum s field) sh -> ST s ()
            bumpField x' (FieldWrapper y) = modifySTRef y (<> x')
            sumField :: (SingI sh) => FieldWrapper (FieldAccum s field) sh -> ST s (FieldWrapper field sh)
            sumField (FieldWrapper x) = sumBuilder <$> readSTRef x
            go :: forall s. ST s (FieldWrapper (Record record) field)
            go = do
                accum <- mapRecordA initField (sumDensePart rb.densePart)
                let
                    bump' :: FieldBuilder record field -> ST s ()
                    bump' (FieldBuilder field builder) = fieldView (bumpField builder) field accum
                traverse_ bump' fields
                mapRecordA sumField accum

-------------- Fill --------------

class GFillRecord metaTT tt (grecord :: Type -> Type) where
    fillGRecord :: (forall sh. Sing sh -> tt @@ sh) -> grecord x
    fillGRecordA :: (Applicative f) => (forall sh. Sing sh -> f (tt @@ sh)) -> f (grecord x)
instance (GFillRecord metaTT tt record) => GFillRecord (M1 i c metaTT) tt (M1 i c record) where
    fillGRecord f = M1 (fillGRecord @metaTT @tt f)
    fillGRecordA f = M1 <$> fillGRecordA @metaTT @tt f
instance (SingI sh, f ~ tt @@ sh) => GFillRecord (K1 R (Proxy sh)) tt (K1 R f) where
    fillGRecord f = K1 (f (sing @sh))
    fillGRecordA f = K1 <$> f (sing @sh)
instance (GFillRecord mf tt f, GFillRecord mg tt g) => GFillRecord (mf :*: mg) tt (f :*: g) where
    fillGRecord f = fillGRecord @mf @tt f :*: fillGRecord @mg @tt f
    fillGRecordA f = liftA2 (:*:) (fillGRecordA @mf @tt f) (fillGRecordA @mg @tt f)
instance GFillRecord U1 f U1 where
    fillGRecord _ = U1
    fillGRecordA _ = pure U1

-------------- Map --------------

class GMapRecord metaTT ttA ttB (gtree1 :: Type -> Type) (gtree2 :: Type -> Type) where
    mapGRecord :: (forall sh. Sing sh -> ttA @@ sh -> ttB @@ sh) -> gtree1 x -> gtree2 x
    traverseGRecord :: (Applicative f) => (forall sh. Sing sh -> ttA @@ sh -> f (ttB @@ sh)) -> gtree1 x -> f (gtree2 x)
instance GMapRecord U1 ttA ttB U1 U1 where
    mapGRecord _ U1 = U1
    traverseGRecord _ U1 = pure U1
instance (GMapRecord metaTT ttA ttB f1 f2) => GMapRecord (M1 i c metaTT) ttA ttB (M1 i c f1) (M1 i c f2) where
    mapGRecord f (M1 x) = M1 (mapGRecord @metaTT @ttA @ttB f x)
    traverseGRecord f (M1 x) = fmap M1 (traverseGRecord @metaTT @ttA @ttB f x)
instance (SingI sh, f1 ~ ttA @@ sh, f2 ~ ttB @@ sh) => GMapRecord (K1 R (Proxy sh)) ttA ttB (K1 R f1) (K1 R f2) where
    mapGRecord f (K1 x) = K1 (f (sing @sh) x)
    traverseGRecord f (K1 x) = fmap K1 (f (sing @sh) x)

instance (GMapRecord mf tt1 tt2 f1 f2, GMapRecord mg tt1 tt2 g1 g2) => GMapRecord (mf :*: mg) tt1 tt2 (f1 :*: g1) (f2 :*: g2) where
    mapGRecord f (x :*: y) = mapGRecord @mf @tt1 @tt2 f x :*: mapGRecord @mg @tt1 @tt2 f y
    traverseGRecord f (x :*: y) = liftA2 (:*:) (traverseGRecord @mf @tt1 @tt2 f x) (traverseGRecord @mg @tt1 @tt2 f y)

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

-------------- Getters --------------

data GenericFieldIndexStep = GfisLeft | GfisRight | GfisIntoM1 Type Meta | GfisIntoK1

class GettersSubtree (prefix :: [GenericFieldIndexStep]) (gettresRep :: Type -> Type) where
    getGetters :: forall x. gettresRep x

instance (GettersSubtree (GfisIntoM1 i c ': path) f) => GettersSubtree path (M1 i c f) where
    getGetters = M1 (getGetters @(GfisIntoM1 i c ': path) @f)

instance (GettersSubtree (GfisLeft ': path) f, GettersSubtree (GfisRight ': path) g) => GettersSubtree path (f :*: g) where
    getGetters = getGetters @(GfisLeft ': path) @f :*: getGetters @(GfisRight ': path) @g

instance GettersSubtree path U1 where
    getGetters = U1

class GenericRecordHasField record (path :: [GenericFieldIndexStep]) subtree where
    gettersGPolyRecord :: (forall x. record x -> subtree)

instance GenericRecordHasField (K1 R f) '[GfisIntoK1] f where
    gettersGPolyRecord (K1 x) = x

instance (GenericRecordHasField f path x) => GenericRecordHasField (f :*: g) (GfisLeft ': path) x where
    gettersGPolyRecord (x :*: _) = gettersGPolyRecord @f @path x

instance (GenericRecordHasField g path x) => GenericRecordHasField (f :*: g) (GfisRight ': path) x where
    gettersGPolyRecord (_ :*: y) = gettersGPolyRecord @g @path y

instance (GenericRecordHasField f path x) => GenericRecordHasField (M1 i c f) (GfisIntoM1 i c ': path) x where
    gettersGPolyRecord (M1 x) = gettersGPolyRecord @f @path x

class
    ( Generic (record leaf)
    , GenericRecordHasField (Rep (record leaf)) (ReverseList rpath '[ 'GfisIntoK1]) (leaf @@ fieldSh)
    ) =>
    GenericRecordHasField' record leaf (rpath :: [GenericFieldIndexStep]) fieldSh fieldRep

instance
    ( Generic (record leaf)
    , GenericRecordHasField (Rep (record leaf)) (ReverseList rpath '[ 'GfisIntoK1]) (leaf @@ fieldSh)
    ) =>
    GenericRecordHasField' record leaf rpath fieldSh fieldRep

type family ReverseList (x :: [k]) (y :: [k]) where
    ReverseList '[] y = y
    ReverseList (x ': xs) y = ReverseList xs (x ': y)

instance
    (SingI sh, forall leaf. GenericRecordHasField' record leaf path sh (K1 R (FieldGetter record sh))) =>
    GettersSubtree path (K1 R (FieldGetter record sh))
    where
    getGetters = K1 (FieldGetter sing go)
      where
        go :: forall leaf. record leaf -> leaf @@ sh
        go = gettersGPolyRecord @(Rep (record leaf)) @(ReverseList path '[ 'GfisIntoK1]) . from
