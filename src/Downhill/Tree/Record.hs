{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Record (
    IsRecord (..),
    Record,
    ShR (..),
    SShR (..),
    fillRecord,
    foldRecord,
    foldMapRecord,
    toListRecord,
    mapRecord,
    zipRecord,
    foldZipRecordEx,
    foldZipRecord,
    fillRecordA,
    recordGetters,
    FieldGetter (..),
    fieldView,
    RecordBuilder,
    fieldBuilder,
    ConstField,
)
where

import Control.Monad.ST.Strict (ST, runST)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Function.Singletons (ConstSym1)
import Data.STRef.Strict (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Singletons (Apply, Proxy, Sing, SingI (sing), TyCon1, withSingI, type (@@), type (~>))
import Downhill.BVar (BVar (BVar, bvarValue))
import Downhill.Linear.AD (AD (..), SparseFun (..), realNode, sparseNode)
import Downhill.Linear.Builder (HasBuilder (..), maybeToMonoid)
import Downhill.Linear.Expr (BiFun (BiFun), Expr (ExprSum), Term, VecPair (..))
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.Tree.BVar (BVarField (..))
import Downhill.Tree.FieldWrapper (
    AdditiveField,
    FieldDualOf,
    FieldGrad,
    FieldScalar,
    FieldTang,
    FieldWrapper (..),
    HasBuilderField,
    ManifoldField,
    ManifoldFieldSh,
    MultiplicativeActionField,
    MultiplicativeField,
    SubtractiveField,
    VSpaceField,
 )
import Downhill.VSpace (VSpace (..))
import NumHask.Prelude

data ProxyField :: leafShape -> Type
type instance Apply ProxyField sh = Proxy sh

data ConstField a :: leafShape -> Type
type instance Apply (ConstField a) sh = a

data FieldGetter record sh = FieldGetter {recordFieldSh :: Sing sh, recordField :: forall f. record f -> f @@ sh}

wrappedRecordField :: FieldGetter record sh -> FieldWrapper (Record f) ('ShR record) -> FieldWrapper f sh
wrappedRecordField getter = FieldWrapper . recordField getter . unFieldWrapper

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

    foldRecordEx :: (forall sh. Sing sh -> a @@ sh -> b -> b) -> b -> record a -> b
    default foldRecordEx ::
        forall a b.
        ( Generic (record a)
        , GFillRecord
            (Rep (record ProxyField))
            a
            (Rep (record a))
        ) =>
        (forall sh. Sing sh -> a @@ sh -> b -> b) ->
        b ->
        record a ->
        b
    foldRecordEx f b = foldMapGRecord2 @(Rep (record ProxyField)) @a f b . from

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

    recordGettersEx :: record (TyCon1 (FieldGetter record))
    default recordGettersEx ::
        ( Generic (record (TyCon1 (FieldGetter record)))
        , GettersSubtree '[] (Rep (record (TyCon1 (FieldGetter record))))
        ) =>
        record (TyCon1 (FieldGetter record))
    recordGettersEx = to (getGetters @'[] @(Rep (record (TyCon1 (FieldGetter record)))))

{-# ANN ShR "HLint: ignore Use newtype instead of data" #-}
data ShR field = ShR ((field ~> Type) -> Type)
data SShR (sh :: ShR field) where
    SShR :: forall record. (IsRecord record) => SShR ('ShR record)

type instance Sing = SShR

instance (IsRecord record) => SingI ('ShR record) where
    sing = SShR

data Record (f :: field ~> Type) :: ShR field ~> Type
type instance Apply (Record field) ('ShR record) = record field

fillRecord ::
    forall record f.
    (IsRecord record) =>
    (forall sh. (SingI sh) => FieldWrapper f sh) ->
    FieldWrapper (Record f) ('ShR record)
fillRecord f = FieldWrapper (fillRecordEx g)
  where
    g :: forall sh. Sing sh -> f @@ sh
    g sh = unFieldWrapper (withSingI sh (f @sh))

fillRecordA ::
    forall record f g.
    (IsRecord record, Applicative g) =>
    (forall sh. (SingI sh) => g (FieldWrapper f sh)) ->
    g (FieldWrapper (Record f) ('ShR record))
fillRecordA f = FieldWrapper <$> fillRecordExA g
  where
    g :: forall sh. Sing sh -> g (f @@ sh)
    g sh = unFieldWrapper <$> withSingI sh (f @sh)

foldRecord :: forall b f sh. (SingI sh) => (forall sh'. (SingI sh') => FieldWrapper f sh' -> b -> b) -> b -> FieldWrapper (Record f) sh -> b
foldRecord f b = case sing @sh of
    SShR -> foldRecordEx f' b . unFieldWrapper
      where
        f' :: forall sh1. Sing sh1 -> f @@ sh1 -> b -> b
        f' sh x = withSingI sh $ f @sh1 (FieldWrapper x)

toListRecord :: forall sh a. (SingI sh) => FieldWrapper (Record (ConstSym1 a)) sh -> [a]
toListRecord (FieldWrapper x) = case sing @sh of
    SShR -> foldRecordEx (const (:)) [] x

foldMapRecordEx ::
    forall record f m.
    (Monoid m, IsRecord record) =>
    (forall sh. Sing sh -> f @@ sh -> m) ->
    record f ->
    m
foldMapRecordEx f = foldRecordEx f' mempty
  where
    f' :: forall sh. Sing sh -> f @@ sh -> m -> m
    f' sh x b = f sh x <> b

foldMapRecord ::
    forall sh f m.
    (SingI sh, Monoid m) =>
    (forall sh'. (SingI sh) => FieldWrapper f sh' -> m) ->
    FieldWrapper (Record f) sh ->
    m
foldMapRecord f (FieldWrapper x) = case sing @sh of
    SShR -> foldMapRecordEx go x
  where
    go :: forall sh'. Sing sh' -> f @@ sh' -> m
    go sh = withSingI sh (f @sh' . FieldWrapper)

mapRecord ::
    forall record a b.
    (IsRecord record) =>
    (forall sh. (SingI sh) => FieldWrapper a sh -> FieldWrapper b sh) ->
    FieldWrapper (Record a) ('ShR record) ->
    FieldWrapper (Record b) ('ShR record)
mapRecord f (FieldWrapper x) = FieldWrapper (mapRecordEx g x)
  where
    g :: forall sh. Sing sh -> a @@ sh -> b @@ sh
    g sh = unFieldWrapper . withSingI sh (f @sh) . FieldWrapper

mapRecordA ::
    forall f record a b.
    (IsRecord record, Applicative f) =>
    (forall sh. (SingI sh) => FieldWrapper a sh -> f (FieldWrapper b sh)) ->
    FieldWrapper (Record a) ('ShR record) ->
    f (FieldWrapper (Record b) ('ShR record))
mapRecordA f (FieldWrapper x) = FieldWrapper <$> mapRecordExA g x
  where
    g :: forall sh. Sing sh -> a @@ sh -> f (b @@ sh)
    g sh = fmap unFieldWrapper . withSingI sh (f @sh) . FieldWrapper

zipRecord ::
    forall sh a b c.
    (SingI sh) =>
    (forall sh'. (SingI sh') => FieldWrapper a sh' -> FieldWrapper b sh' -> FieldWrapper c sh') ->
    FieldWrapper (Record a) sh ->
    FieldWrapper (Record b) sh ->
    FieldWrapper (Record c) sh
zipRecord f (FieldWrapper x) (FieldWrapper y) = case sing @sh of
    SShR -> FieldWrapper (zipRecordEx g x y)
  where
    g :: forall sh'. Sing sh' -> a @@ sh' -> b @@ sh' -> c @@ sh'
    g sh x' y' = unFieldWrapper (withSingI sh (f @sh' (FieldWrapper x') (FieldWrapper y')))

foldZipRecordEx :: forall record m a b. (Monoid m, IsRecord record) => (forall sh. Sing sh -> a @@ sh -> b @@ sh -> m) -> record a -> record b -> m
foldZipRecordEx f x y = foldMapRecordEx (const id) zipped
  where
    zipped :: record (ConstSym1 m)
    zipped = zipRecordEx f x y

foldZipRecord ::
    forall m a b sh.
    (Monoid m, SingI sh) =>
    (forall sh'. (SingI sh') => FieldWrapper a sh' -> FieldWrapper b sh' -> m) ->
    FieldWrapper (Record a) sh ->
    FieldWrapper (Record b) sh ->
    m
foldZipRecord f (FieldWrapper x) (FieldWrapper y) = case sing @sh of
    SShR -> foldZipRecordEx f' x y
      where
        f' :: forall sh1. Sing sh1 -> a @@ sh1 -> b @@ sh1 -> m
        f' sh x' y' = withSingI sh $ f @sh1 (FieldWrapper x') (FieldWrapper y')

fieldView ::
    (IsRecord record) =>
    ((SingI sh) => FieldWrapper a sh -> r) ->
    FieldGetter record sh ->
    FieldWrapper (Record a) ('ShR record) ->
    r
fieldView f (FieldGetter sh field) (FieldWrapper record) = withSingI sh $ f (FieldWrapper (field record))

recordGetters :: forall record. (IsRecord record) => FieldWrapper (Record (TyCon1 (FieldGetter record))) ('ShR record)
recordGetters = FieldWrapper recordGettersEx

instance (SingI sh, AdditiveField field) => Additive (FieldWrapper (Record field) sh) where
    zero = case sing @sh of
        SShR -> fillRecord zero
    (+) = case sing @sh of
        SShR -> zipRecord (+)

instance (SingI sh, SubtractiveField field) => Subtractive (FieldWrapper (Record field) sh) where
    negate = case sing @sh of
        SShR -> mapRecord negate
    (-) = case sing @sh of
        SShR -> zipRecord (-)

instance (SingI sh, MultiplicativeField field) => Multiplicative (FieldWrapper (Record field) sh) where
    one = case sing @sh of
        SShR -> fillRecord one
    (*) = case sing @sh of
        SShR -> zipRecord (*)

instance (SingI sh, MultiplicativeActionField field) => MultiplicativeAction (FieldWrapper (Record field) sh) where
    type Scalar (FieldWrapper (Record field) sh) = FieldScalar field
    v |* a = case sing @sh of
        SShR -> mapRecord (|* a) v

data FieldBuilder record field = forall sh. FieldBuilder (FieldGetter record sh) (VecBuilder (FieldWrapper field sh))

fieldBuilder :: FieldGetter record sh -> VecBuilder (FieldWrapper field sh) -> VecBuilder (FieldWrapper (Record field) ('ShR record))
fieldBuilder getter builder = RecordBuilder Nothing (DList.singleton (FieldBuilder getter builder))

data FieldAccum s (field :: sh ~> Type) :: sh ~> Type
type instance Apply (FieldAccum s field) sh = STRef s (VecBuilder (FieldWrapper field sh))

data RecordBuilder sh field where
    RecordBuilder ::
        (Maybe (Sum (FieldWrapper (Record field) ('ShR record)))) ->
        (DList (FieldBuilder record field)) ->
        RecordBuilder ('ShR record) field

instance (SingI sh, AdditiveField field) => Semigroup (RecordBuilder sh field) where
    RecordBuilder x1 y1 <> RecordBuilder x2 y2 = RecordBuilder (x1 <> x2) (y1 <> y2)

instance (SingI sh, AdditiveField field) => Monoid (RecordBuilder sh field) where
    mempty = case sing @sh of SShR -> RecordBuilder mempty mempty

sumDensePart ::
    (IsRecord record, forall sh. (SingI sh) => HasBuilder (FieldWrapper field sh)) =>
    Maybe (Sum (FieldWrapper (Record field) ('ShR record))) -> FieldWrapper (Record field) ('ShR record)
sumDensePart = getSum . maybeToMonoid

sumRBuilder ::
    forall field record.
    (IsRecord record, forall sh. (SingI sh) => HasBuilder (FieldWrapper field sh)) =>
    RecordBuilder ('ShR record) field -> FieldWrapper (Record field) ('ShR record)
sumRBuilder (RecordBuilder densePart fields) = case toList fields of
    [] -> sumDensePart densePart -- don't descend into fields, keep identityBuilder quick
    _ -> runST go
      where
        initField :: (SingI sh) => FieldWrapper field sh -> ST s (FieldWrapper (FieldAccum s field) sh)
        initField x = FieldWrapper <$> newSTRef (identityBuilder x)
        bumpField :: (SingI sh) => VecBuilder (FieldWrapper field sh) -> FieldWrapper (FieldAccum s field) sh -> ST s ()
        bumpField x' (FieldWrapper y) = modifySTRef y (<> x')
        sumField :: (SingI sh) => FieldWrapper (FieldAccum s field) sh -> ST s (FieldWrapper field sh)
        sumField (FieldWrapper x) = sumBuilder <$> readSTRef x
        go :: forall s. ST s (FieldWrapper (Record field) ('ShR record))
        go = do
            accum <- mapRecordA initField (sumDensePart densePart)
            let
                bump' :: FieldBuilder record field -> ST s ()
                bump' (FieldBuilder field builder) = fieldView (bumpField builder) field accum
            traverse_ bump' fields
            mapRecordA sumField accum

instance
    ( AdditiveField field
    , SingI sh
    , forall sh'. (SingI sh') => HasBuilder (FieldWrapper field sh')
    ) =>
    HasBuilder (FieldWrapper (Record field) sh)
    where
    type VecBuilder (FieldWrapper (Record field) sh) = RecordBuilder sh field
    identityBuilder x = case sing @sh of
        SShR -> RecordBuilder (Just (Sum x)) mempty
    sumBuilder = case sing @sh of
        SShR -> sumRBuilder

type instance FieldScalar (Record a) = FieldScalar a
type instance FieldDualOf (Record a) = Record (FieldDualOf a)

instance (VSpaceField a, SingI sh) => VSpace (FieldWrapper (Record a) sh) where
    dv |*| v = getSum (foldMapRecord @sh @(ConstField (Sum (FieldScalar a))) @(Sum (FieldScalar a)) sumField y)
      where
        sumField :: forall sh'. FieldWrapper (ConstField (Sum (FieldScalar a))) sh' -> Sum (FieldScalar a)
        sumField = unFieldWrapper
        y :: FieldWrapper (Record (ConstField (Sum (FieldScalar a)))) sh
        y = case sing @sh of
            SShR -> zipRecord zipField dv v
              where
                zipField ::
                    (SingI sh') =>
                    FieldWrapper (FieldDualOf a) sh' ->
                    FieldWrapper a sh' ->
                    FieldWrapper (ConstField (Sum (FieldScalar a))) sh'
                zipField x' y' = FieldWrapper (Sum (x' |*| y'))

data TangSym :: Type ~> Type
type instance Apply TangSym a = Tang a

-- data TangRecord (r :: (fieldType ~> Type) -> Type) :: (fieldType ~> Type) ~> Type
-- type instance Apply (TangRecord r) field = r (TangSym .@#@$$$ field)

type instance FieldTang (Record a) = Record (FieldTang a)

instance
    ( ManifoldField a
    , SingI sh
    , Subtractive (FieldScalar (FieldTang a))
    ) =>
    Manifold (FieldWrapper (Record a) sh)
    where
    (.+~^) = zipRecord (.+~^)
    (.-~^) = zipRecord (.-~^)

getBVarField ::
    forall record a sh r.
    ( IsRecord record
    , ManifoldField a
    , HasBuilderField (FieldTang a)
    , HasBuilderField (FieldGrad a)
    ) =>
    BVar r (FieldWrapper (Record a) ('ShR record)) -> FieldGetter record sh -> BVar r (FieldWrapper a sh)
getBVarField (BVar x dx) field = fieldView go field x
  where
    go :: (SingI sh, ManifoldFieldSh a sh) => FieldWrapper a sh -> BVar r (FieldWrapper a sh)
    go y = BVar y (sparseNode f dx)
      where
        f ::
            SparseFun
                ( 'VecPair
                    (FieldWrapper (Record (FieldTang a)) ('ShR record))
                    (FieldWrapper (Record (FieldDualOf (FieldTang a))) ('ShR record))
                )
                ( 'VecPair
                    (Tang (FieldWrapper a sh))
                    (DualOf (Tang (FieldWrapper a sh)))
                )
        f = SparseFun fwdF backF
        fwdF :: FieldWrapper (Record (FieldTang a)) ('ShR record) -> VecBuilder (FieldWrapper (FieldTang a) sh)
        fwdF z = identityBuilder $ wrappedRecordField field z
        backF :: VecBuilder (FieldWrapper (FieldDualOf (FieldTang a)) sh) -> RecordBuilder ('ShR record) (FieldDualOf (FieldTang a))
        backF dz = RecordBuilder Nothing (DList.singleton (FieldBuilder field dz))

splitBVarRecord ::
    forall a b sh r.
    ( ManifoldField a
    , HasBuilderField (FieldTang a)
    , HasBuilderField (FieldGrad a)
    , SingI sh
    ) =>
    (forall sh'. (SingI sh') => BVar r (FieldWrapper a sh') -> FieldWrapper b sh') ->
    BVar r (FieldWrapper (Record a) sh) ->
    FieldWrapper (Record b) sh
splitBVarRecord f x = case sing @sh of
    SShR -> mapRecord (f . getBVarField x . unFieldWrapper) recordGetters

data BVarFieldSym (r :: Type) (a :: k ~> Type) :: k ~> Type
type instance Apply (BVarFieldSym r a) sh = BVar r (FieldWrapper a sh)

joinBVarRecord' ::
    forall r a b record.
    ( IsRecord record
    , AdditiveField (FieldGrad a)
    , AdditiveField (FieldTang a)
    , HasBuilderField (FieldTang a)
    , HasBuilderField (FieldGrad a)
    ) =>
    (forall sh'. (SingI sh') => FieldWrapper b sh' -> BVar r (FieldWrapper a sh')) ->
    FieldWrapper (Record b) ('ShR record) ->
    BVar r (FieldWrapper (Record a) ('ShR record))
joinBVarRecord' f xrec = BVar y dy
  where
    brec :: FieldWrapper (Record (BVarFieldSym r a)) ('ShR record)
    brec = mapRecord (FieldWrapper . f) xrec
    y :: FieldWrapper (Record a) ('ShR record)
    y = mapRecord (bvarValue . unFieldWrapper) brec
    dy :: AD r (FieldWrapper (Record (FieldTang a)) ('ShR record))
    dy = realNode (ExprSum (toList (foldZipRecord mkTerms brec recordGetters)))
    mkTerms ::
        forall sh'.
        (SingI sh') =>
        FieldWrapper (BVarFieldSym r a) sh' ->
        FieldWrapper (TyCon1 (FieldGetter record)) sh' ->
        DList
            ( Term
                ('VecPair r (DualOf r))
                ('VecPair (FieldWrapper (Record (FieldTang a)) ('ShR record)) (FieldWrapper (Record (FieldGrad a)) ('ShR record)))
            )
    mkTerms (FieldWrapper (BVar _ (AD dz))) (FieldWrapper getter) = DList.singleton (dz (BiFun fwd back))
      where
        fwd :: FieldWrapper (FieldTang a) sh' -> RecordBuilder ('ShR record) (FieldTang a)
        fwd x = fieldBuilder getter (identityBuilder x)
        back :: FieldWrapper (Record (FieldGrad a)) ('ShR record) -> VecBuilder (FieldWrapper (FieldGrad a) sh')
        back x = identityBuilder (wrappedRecordField getter x)

joinBVarRecord ::
    forall a b sh r.
    ( SingI sh
    , ManifoldField a
    , HasBuilderField (FieldTang a)
    , HasBuilderField (FieldGrad a)
    ) =>
    (forall sh'. (SingI sh') => FieldWrapper b sh' -> BVar r (FieldWrapper a sh')) ->
    FieldWrapper (Record b) sh ->
    BVar r (FieldWrapper (Record a) sh)
joinBVarRecord = case sing @sh of
    SShR -> joinBVarRecord'

-------------- Fill --------------

class GFillRecord metaTT tt (grecord :: Type -> Type) where
    fillGRecord :: (forall sh. Sing sh -> tt @@ sh) -> grecord x
    fillGRecordA :: (Applicative f) => (forall sh. Sing sh -> f (tt @@ sh)) -> f (grecord x)
    foldMapGRecord :: (Monoid m) => (forall sh. Sing sh -> tt @@ sh -> m) -> grecord x -> m
    foldMapGRecord2 :: (forall sh. Sing sh -> tt @@ sh -> b -> b) -> b -> grecord x -> b
instance (GFillRecord metaTT tt record) => GFillRecord (M1 i c metaTT) tt (M1 i c record) where
    fillGRecord f = M1 (fillGRecord @metaTT @tt f)
    fillGRecordA f = M1 <$> fillGRecordA @metaTT @tt f
    foldMapGRecord f (M1 x) = foldMapGRecord @metaTT @tt f x
    foldMapGRecord2 f b (M1 x) = foldMapGRecord2 @metaTT @tt f b x
instance (SingI sh, f ~ tt @@ sh) => GFillRecord (K1 R (Proxy sh)) tt (K1 R f) where
    fillGRecord f = K1 (f (sing @sh))
    fillGRecordA f = K1 <$> f (sing @sh)
    foldMapGRecord f (K1 x) = f (sing @sh) x
    foldMapGRecord2 f b (K1 x) = f (sing @sh) x b
instance (GFillRecord mf tt f, GFillRecord mg tt g) => GFillRecord (mf :*: mg) tt (f :*: g) where
    fillGRecord f = fillGRecord @mf @tt f :*: fillGRecord @mg @tt f
    fillGRecordA f = liftA2 (:*:) (fillGRecordA @mf @tt f) (fillGRecordA @mg @tt f)
    foldMapGRecord f (x :*: y) = foldMapGRecord @mf @tt f x <> foldMapGRecord @mg @tt f y
    foldMapGRecord2 f b (x :*: y) = foldMapGRecord2 @mf @tt f (foldMapGRecord2 @mg @tt f b y) x
instance GFillRecord U1 f U1 where
    fillGRecord _ = U1
    fillGRecordA _ = pure U1
    foldMapGRecord _ U1 = mempty
    foldMapGRecord2 _ b U1 = b

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

instance
    ( BVarField f
    , ManifoldField f
    , HasBuilderField (FieldTang f)
    , HasBuilderField (FieldGrad f)
    ) =>
    BVarField (Record f)
    where
    type FieldBVar r (Record f) = Record (FieldBVar r f)
    splitField = splitBVarRecord splitField
    joinField = joinBVarRecord joinField