{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Record (
    -- * Record metadata
    NamedFields (..),

    -- * Records
    IsRecord (..),
    Record,
    RecordValue (..),
    ShR (..),
    SShR (..),
    fillRecordExA,
    toListRecordEx,
    foldZipRecordEx,
    recordGetters,
    FieldGetter (..),
    fieldView,
    RecordBuilder,
    RecordValueBuilder,
    recordValueFieldBuilder,
    getBVarField,
    splitBVarRecord,
    joinBVarRecord,
    ConstField,
    EmptyRecord (..),
)
where

import Control.Monad.ST.Strict (ST, runST)
import Data.Constraint (Dict (Dict))
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Function.Singletons (ConstSym1)
import Data.Proxy (Proxy (Proxy))
import Data.STRef.Strict (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Singletons (Apply, Sing, SingI (sing), TyCon1, withSingI, type (@@), type (~>))
import Data.Text (Text)
import Data.Text qualified as Text
import Downhill.BVar (BVar (BVar, bvarValue))
import Downhill.Linear.AD (AD (..), projectNode, realNode)
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Linear.Expr (BiFun (BiFun), Expr (ExprSum), Term, VecPair (..))
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.Tree.At (
    AdditiveRepr,
    At (..),
    MultiplicativeActionRepr,
    MultiplicativeRepr,
    SubtractiveRepr,
    unAt,
    pattern At,
 )
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.Representation (ReprDualOf, ReprGrad, ReprScalar, ReprTang)
import Downhill.Tree.Representation qualified as Representation
import Downhill.VSpace (VSpace (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import NumHask.Prelude

data ProxyField :: leafShape -> Type
type instance Apply ProxyField sh = Proxy sh

data ConstField a :: leafShape -> Type
type instance Apply (ConstField a) sh = a

-- | The names of all fields in a record, preserving its field layout.
newtype NamedFields fields = NamedFields {getNamedFields :: fields}

data FieldGetter record sh = FieldGetter {recordFieldSh :: Sing sh, recordField :: forall f. record f -> f @@ sh}

type IsRecord :: forall {fieldSh}. ((fieldSh ~> Type) -> Type) -> Constraint

{- | A finite product of fields with shapes.

An @IsRecord@ type is a container parameterized by a representation
@repr :: fieldSh ~> Type@. Each member stores a value @repr @@ sh@ for some
field shape @sh@, but the record itself may be an ordinary Haskell record,
a tuple-like type, or a homogeneous array. The fields are not wrapped by the
library, so vanilla records keep their usual ergonomics, including record-dot
access.

The class is intentionally small: it says that the container can be filled,
folded, mapped, zipped, and can provide field getters. Generic deriving is
available for ordinary records whose fields are all representation
applications; hand-written instances are useful for other containers such as
arrays. A record may additionally expose field names through 'fieldNames';
names are metadata and do not affect these structural operations.

A polymorphic function constrained only by @IsRecord record@ can inspect the
finite product structure and its optional field names, but it does not get
pretty-printing or other presentation-specific metadata.
-}
class IsRecord (record :: (fieldSh ~> Type) -> Type) where
    -- | Names of the fields, or 'Nothing' for a positional record.
    fieldNames :: Maybe (NamedFields (record (ConstField Text)))
    default fieldNames ::
        ( Generic (record (ConstField Text))
        , GRecordFieldNames (Rep (record (ConstField Text)))
        ) =>
        Maybe (NamedFields (record (ConstField Text)))
    fieldNames = NamedFields . to <$> genericRecordFieldNames @(Rep (record (ConstField Text)))

    fillRecordEx :: (forall sh. Sing sh -> f @@ sh) -> record f
    default fillRecordEx ::
        forall f.
        ( Generic (record f)
        , GFillRecord (Rep (record ProxyField)) f (Rep (record f))
        ) =>
        (forall sh. Sing sh -> f @@ sh) ->
        record f
    fillRecordEx = to . fillGRecord @(Rep (record ProxyField)) @f

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

{- | The kind of shapes for record nodes.

A leaf shape describes one field. 'ShR' lifts a whole @IsRecord@ container
into a shape, so the tree can treat an entire record as one node:

> 'ShR MyRecord :: ShR fieldSh

The singleton for @'ShR record@ carries the corresponding 'IsRecord'
instance. That is how operations on wrapped tree values can rediscover the
record algebra from the shape alone.
-}
data ShR field = ShR ((field ~> Type) -> Type)

data SShR (sh :: ShR field) where
    SShR :: forall record. (IsRecord record) => SShR ('ShR record)

type instance Sing = SShR

instance (IsRecord record) => SingI ('ShR record) where
    sing = SShR

{- | Adapter that lets tree internals store records as representation nodes.

Most tree code works with a representation @repr :: field ~> Type@ applied
at some shape. A record type has the opposite shape: it is a container
parameterized by a representation, @record repr@. 'Record' turns such a
container into an ordinary representation by making the record type part of
the shape:

> Record repr @@ 'ShR MyRecord = MyRecord repr

This type is mostly an implementation detail of the tree layer; users
usually name the record type and its 'ShR' shape instead.
-}
data Record (f :: field ~> Type) :: ShR field ~> Type

type instance Apply (Record field) ('ShR record) = record field

{- | A complete record value with a concrete field representation.

This wrapper owns aggregate instances such as 'HasBuilder'. Unlike 'At',
it represents a record directly rather than encoding one as a synthetic
representation application at an @'ShR record@ shape.
-}
newtype RecordValue record field = RecordValue {unRecordValue :: record field}

deriving instance (Eq (record field)) => Eq (RecordValue record field)
deriving instance (Show (record field)) => Show (RecordValue record field)

fillRecordExA :: forall record g f. (IsRecord record, Applicative g) => (forall sh. Sing sh -> g (f @@ sh)) -> g (record f)
fillRecordExA f = mapRecordExA (\sh _ -> f sh) proxyRecord
  where
    proxyRecord :: record ProxyField
    proxyRecord = fillRecordEx (\(_ :: Sing sh) -> Proxy @sh)

fillRecord ::
    forall record f.
    (IsRecord record) =>
    (forall sh. (SingI sh) => At f sh) ->
    At (Record f) ('ShR record)
fillRecord f = At (fillRecordEx g)
  where
    g :: forall sh. Sing sh -> f @@ sh
    g sh = unAt (withSingI sh (f @sh))

fillRecordA ::
    forall record f g.
    (IsRecord record, Applicative g) =>
    (forall sh. (SingI sh) => g (At f sh)) ->
    g (At (Record f) ('ShR record))
fillRecordA f = At <$> fillRecordExA g
  where
    g :: forall sh. Sing sh -> g (f @@ sh)
    g sh = unAt <$> withSingI sh (f @sh)

toListRecordEx :: forall sh a. Sing sh -> Record (ConstSym1 a) @@ sh -> [a]
toListRecordEx SShR = foldRecordEx (const (:)) []

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

mapRecord ::
    forall record a b.
    (IsRecord record) =>
    (forall sh. (SingI sh) => At a sh -> At b sh) ->
    At (Record a) ('ShR record) ->
    At (Record b) ('ShR record)
mapRecord f (At x) = At (mapRecordEx g x)
  where
    g :: forall sh. Sing sh -> a @@ sh -> b @@ sh
    g sh = unAt . withSingI sh (f @sh) . At

mapRecordA ::
    forall f record a b.
    (IsRecord record, Applicative f) =>
    (forall sh. (SingI sh) => At a sh -> f (At b sh)) ->
    At (Record a) ('ShR record) ->
    f (At (Record b) ('ShR record))
mapRecordA f (At x) = At <$> mapRecordExA g x
  where
    g :: forall sh. Sing sh -> a @@ sh -> f (b @@ sh)
    g sh = fmap unAt . withSingI sh (f @sh) . At

zipRecord ::
    forall sh a b c.
    (SingI sh) =>
    (forall sh'. (SingI sh') => At a sh' -> At b sh' -> At c sh') ->
    At (Record a) sh ->
    At (Record b) sh ->
    At (Record c) sh
zipRecord f (At x) (At y) = case sing @sh of
    SShR -> At (zipRecordEx g x y)
  where
    g :: forall sh'. Sing sh' -> a @@ sh' -> b @@ sh' -> c @@ sh'
    g sh x' y' = unAt (withSingI sh (f @sh' (At x') (At y')))

foldZipRecordEx :: forall record m a b. (Monoid m, IsRecord record) => (forall sh. Sing sh -> a @@ sh -> b @@ sh -> m) -> record a -> record b -> m
foldZipRecordEx f x y = foldMapRecordEx (const id) zipped
  where
    zipped :: record (ConstSym1 m)
    zipped = zipRecordEx f x y

fieldView ::
    (IsRecord record) =>
    ((SingI sh) => At a sh -> r) ->
    FieldGetter record sh ->
    At (Record a) ('ShR record) ->
    r
fieldView f (FieldGetter sh field) (At record) = withSingI sh $ f (At (field record))

recordGetters :: forall record. (IsRecord record) => At (Record (TyCon1 (FieldGetter record))) ('ShR record)
recordGetters = At recordGettersEx

instance (SingI sh, AdditiveRepr field) => Additive (At (Record field) sh) where
    zero = case sing @sh of
        SShR -> fillRecord zero
    (+) = case sing @sh of
        SShR -> zipRecord (+)

instance (SingI sh, SubtractiveRepr field) => Subtractive (At (Record field) sh) where
    negate = case sing @sh of
        SShR -> mapRecord negate
    (-) = case sing @sh of
        SShR -> zipRecord (-)

instance (SingI sh, MultiplicativeRepr field) => Multiplicative (At (Record field) sh) where
    one = case sing @sh of
        SShR -> fillRecord one
    (*) = case sing @sh of
        SShR -> zipRecord (*)

instance (SingI sh, MultiplicativeActionRepr field) => MultiplicativeAction (At (Record field) sh) where
    type Scalar (At (Record field) sh) = ReprScalar field
    v |* a = case sing @sh of
        SShR -> mapRecord (|* a) v

data FieldBuilder record field = forall sh. FieldBuilder (FieldGetter record sh) (VecBuilder (At field sh))

data FieldAccum s (field :: sh ~> Type) :: sh ~> Type
type instance Apply (FieldAccum s field) sh = STRef s (VecBuilder (At field sh))

data RecordBuilder sh field where
    RecordBuilder ::
        (Maybe (Sum (At (Record field) ('ShR record)))) ->
        (DList (FieldBuilder record field)) ->
        RecordBuilder ('ShR record) field

instance (SingI sh, AdditiveRepr field) => Semigroup (RecordBuilder sh field) where
    RecordBuilder x1 y1 <> RecordBuilder x2 y2 = RecordBuilder (x1 <> x2) (y1 <> y2)

instance (SingI sh, AdditiveRepr field) => Monoid (RecordBuilder sh field) where
    mempty = case sing @sh of SShR -> RecordBuilder mempty mempty

instance
    (IsRecord record, Representation.AdditiveRepr field) =>
    Additive (RecordValue record field)
    where
    zero = RecordValue (fillRecordEx zeroField)
      where
        zeroField :: forall sh. Sing sh -> field @@ sh
        zeroField sh = case Representation.allConstrained @Additive @field sh of
            Dict -> zero
    RecordValue x + RecordValue y = RecordValue (zipRecordEx addField x y)
      where
        addField :: forall sh. Sing sh -> field @@ sh -> field @@ sh -> field @@ sh
        addField sh = case Representation.allConstrained @Additive @field sh of
            Dict -> (+)

instance
    (IsRecord record, Representation.SubtractiveRepr field) =>
    Subtractive (RecordValue record field)
    where
    negate (RecordValue x) = RecordValue (mapRecordEx negateField x)
      where
        negateField :: forall sh. Sing sh -> field @@ sh -> field @@ sh
        negateField sh = case Representation.allConstrained @Subtractive @field sh of
            Dict -> negate
    RecordValue x - RecordValue y = RecordValue (zipRecordEx subtractField x y)
      where
        subtractField :: forall sh. Sing sh -> field @@ sh -> field @@ sh -> field @@ sh
        subtractField sh = case Representation.allConstrained @Subtractive @field sh of
            Dict -> (-)

instance
    (IsRecord record, Representation.MultiplicativeActionRepr field) =>
    MultiplicativeAction (RecordValue record field)
    where
    type Scalar (RecordValue record field) = ReprScalar field
    RecordValue x |* scalar = RecordValue (mapRecordEx scaleField x)
      where
        scaleField :: forall sh. Sing sh -> field @@ sh -> field @@ sh
        scaleField sh = case Representation.allConstrained @(Representation.MultiplicativeActionOver (ReprScalar field)) @field sh of
            Dict -> (|* scalar)

type instance DualOf (RecordValue record field) = RecordValue record (ReprDualOf field)

instance
    (IsRecord record, Representation.VSpaceRepr field) =>
    VSpace (RecordValue record field)
    where
    RecordValue dx |*| RecordValue x =
        getSum (foldZipRecordEx pairField dx x)
      where
        pairField ::
            forall sh.
            Sing sh ->
            ReprDualOf field @@ sh ->
            field @@ sh ->
            Sum (ReprScalar field)
        pairField sh = case Representation.vspaceDict @field sh of
            Dict -> \dx' x' -> Sum (dx' |*| x')

type instance Tang (RecordValue record field) = RecordValue record (ReprTang field)

instance
    (IsRecord record, Representation.ManifoldRepr field) =>
    Manifold (RecordValue record field)
    where
    RecordValue point .+~^ RecordValue tangent =
        RecordValue (zipRecordEx addTangent point tangent)
      where
        addTangent ::
            forall sh.
            Sing sh ->
            field @@ sh ->
            ReprTang field @@ sh ->
            field @@ sh
        addTangent sh = case Representation.manifoldDict @field sh of
            Dict -> (.+~^)
    RecordValue point .-~^ RecordValue tangent =
        RecordValue (zipRecordEx subtractTangent point tangent)
      where
        subtractTangent ::
            forall sh.
            Sing sh ->
            field @@ sh ->
            ReprTang field @@ sh ->
            field @@ sh
        subtractTangent sh = case Representation.manifoldDict @field sh of
            Dict -> (.-~^)

data RecordValueFieldBuilder record field where
    RecordValueFieldBuilder ::
        FieldGetter record sh ->
        VecBuilder (field @@ sh) ->
        RecordValueFieldBuilder record field

data RecordValueBuilder record field
    = RecordValueBuilder
        (Maybe (Sum (RecordValue record field)))
        (DList (RecordValueFieldBuilder record field))

instance
    (IsRecord record, Representation.BuilderRepr field) =>
    Semigroup (RecordValueBuilder record field)
    where
    RecordValueBuilder x xs <> RecordValueBuilder y ys =
        RecordValueBuilder (x <> y) (xs <> ys)

instance
    (IsRecord record, Representation.BuilderRepr field) =>
    Monoid (RecordValueBuilder record field)
    where
    mempty = RecordValueBuilder mempty mempty

recordValueFieldBuilder ::
    FieldGetter record sh ->
    VecBuilder (field @@ sh) ->
    RecordValueBuilder record field
recordValueFieldBuilder getter builder =
    RecordValueBuilder Nothing (DList.singleton (RecordValueFieldBuilder getter builder))

data RecordValueAccum s (field :: k ~> Type) :: k ~> Type
type instance Apply (RecordValueAccum s field) sh = STRef s (VecBuilder (field @@ sh))

sumRecordValueBuilder ::
    forall record field.
    (IsRecord record, Representation.BuilderRepr field) =>
    RecordValueBuilder record field ->
    RecordValue record field
sumRecordValueBuilder (RecordValueBuilder densePart sparseParts) =
    case toList sparseParts of
        [] -> denseValue
        _ -> runST sumSparse
  where
    denseValue :: RecordValue record field
    denseValue = case densePart of
        Nothing -> zero
        Just (Sum value) -> value

    sumSparse :: forall s. ST s (RecordValue record field)
    sumSparse = do
        accumulators <- mapRecordExA initField (unRecordValue denseValue)
        traverse_ (addField accumulators) sparseParts
        RecordValue <$> mapRecordExA sumField accumulators

    initField ::
        forall s sh.
        Sing sh ->
        field @@ sh ->
        ST s (RecordValueAccum s field @@ sh)
    initField sh value = case Representation.allConstrained @HasBuilder @field sh of
        Dict -> newSTRef (identityBuilder value)

    addField ::
        forall s.
        record (RecordValueAccum s field) ->
        RecordValueFieldBuilder record field ->
        ST s ()
    addField accumulators (RecordValueFieldBuilder getter builder) =
        case Representation.allConstrained @HasBuilder @field (recordFieldSh getter) of
            Dict -> modifySTRef (recordField getter accumulators) (<> builder)

    sumField ::
        forall s sh.
        Sing sh ->
        RecordValueAccum s field @@ sh ->
        ST s (field @@ sh)
    sumField sh accumulator = case Representation.allConstrained @HasBuilder @field sh of
        Dict -> sumBuilder <$> readSTRef accumulator

instance
    (IsRecord record, Representation.BuilderRepr field) =>
    HasBuilder (RecordValue record field)
    where
    type VecBuilder (RecordValue record field) = RecordValueBuilder record field
    identityBuilder value = RecordValueBuilder (Just (Sum value)) mempty
    sumBuilder = sumRecordValueBuilder

type instance ReprScalar (Record a) = ReprScalar a
type instance ReprDualOf (Record a) = Record (ReprDualOf a)

data TangSym :: Type ~> Type
type instance Apply TangSym a = Tang a

-- data TangRecord (r :: (fieldSh ~> Type) -> Type) :: (fieldSh ~> Type) ~> Type
-- type instance Apply (TangRecord r) field = r (TangSym .@#@$$$ field)

type instance ReprTang (Record a) = Record (ReprTang a)

getBVarField ::
    forall record a sh r.
    ( IsRecord record
    , Representation.HasGradRepr a
    ) =>
    BVar r (RecordValue record a) ->
    FieldGetter record sh ->
    BVar r (a @@ sh)
getBVarField (BVar (RecordValue values) derivative) getter =
    case Representation.hasGradReprDict @a (recordFieldSh getter) of
        Dict -> BVar (recordField getter values) (projectNode projectTangent injectGradient derivative)
  where
    projectTangent ::
        RecordValue record (ReprTang a) ->
        ReprTang a @@ sh
    projectTangent (RecordValue tangents) = recordField getter tangents

    injectGradient ::
        VecBuilder (ReprGrad a @@ sh) ->
        RecordValueBuilder record (ReprGrad a)
    injectGradient = recordValueFieldBuilder getter

splitBVarRecord ::
    forall a record r.
    ( IsRecord record
    , BVarRepr a
    , Representation.HasGradRepr a
    ) =>
    BVar r (RecordValue record a) ->
    record (ReprBVar r a)
splitBVarRecord value = mapRecordEx splitFieldValue (recordGettersEx @record)
  where
    splitFieldValue ::
        forall sh.
        Sing sh ->
        FieldGetter record sh ->
        ReprBVar r a @@ sh
    splitFieldValue sh getter =
        splitBVarField @a sh (getBVarField value getter)

data BVarValue (r :: Type) (a :: k ~> Type) :: k ~> Type
type instance Apply (BVarValue r a) sh = BVar r (a @@ sh)

joinBVarRecord ::
    forall r a record.
    ( IsRecord record
    , BVarRepr a
    , Representation.HasGradRepr a
    ) =>
    record (ReprBVar r a) ->
    BVar r (RecordValue record a)
joinBVarRecord fields = BVar value derivative
  where
    joinedFields :: record (BVarValue r a)
    joinedFields = mapRecordEx joinFieldValue fields

    joinFieldValue ::
        forall sh.
        Sing sh ->
        ReprBVar r a @@ sh ->
        BVar r (a @@ sh)
    joinFieldValue sh = joinBVarField @a sh

    value :: RecordValue record a
    value = RecordValue (mapRecordEx (const bvarValue) joinedFields)

    derivative :: AD r (RecordValue record (ReprTang a))
    derivative =
        realNode
            (ExprSum (toList (foldZipRecordEx fieldTerms joinedFields (recordGettersEx @record))))

    fieldTerms ::
        forall sh.
        Sing sh ->
        BVar r (a @@ sh) ->
        FieldGetter record sh ->
        DList
            ( Term
                ('VecPair r (DualOf r))
                ( 'VecPair
                    (RecordValue record (ReprTang a))
                    (RecordValue record (ReprGrad a))
                )
            )
    fieldTerms sh (BVar _ (AD childDerivative)) getter =
        case Representation.hasGradReprDict @a sh of
            Dict -> DList.singleton (childDerivative (BiFun injectTangent projectGradient))
      where
        injectTangent ::
            ReprTang a @@ sh ->
            RecordValueBuilder record (ReprTang a)
        injectTangent tangent =
            case Representation.allConstrained @HasBuilder @(ReprTang a) sh of
                Dict -> recordValueFieldBuilder getter (identityBuilder tangent)

        projectGradient ::
            RecordValue record (ReprGrad a) ->
            VecBuilder (ReprGrad a @@ sh)
        projectGradient (RecordValue gradients) =
            case Representation.allConstrained @HasBuilder @(ReprGrad a) sh of
                Dict -> identityBuilder (recordField getter gradients)

data EmptyRecord f = EmptyRecord

instance IsRecord EmptyRecord where
    fieldNames = Nothing
    fillRecordEx _ = EmptyRecord
    foldRecordEx _ b _ = b
    mapRecordEx _ _ = EmptyRecord
    mapRecordExA _ _ = pure EmptyRecord
    zipRecordEx _ _ _ = EmptyRecord
    recordGettersEx = EmptyRecord

-------------- Field names --------------

class GRecordFieldNames (record :: Type -> Type) where
    genericRecordFieldNames :: forall p. Maybe (record p)

instance
    (GNamedRecordFields fields) =>
    GRecordFieldNames (M1 D metadata (M1 C ('MetaCons name fixity 'True) fields))
    where
    genericRecordFieldNames = Just (M1 (M1 genericNamedRecordFields))

instance GRecordFieldNames (M1 D metadata (M1 C ('MetaCons name fixity 'False) fields)) where
    genericRecordFieldNames = Nothing

class GNamedRecordFields (fields :: Type -> Type) where
    genericNamedRecordFields :: forall p. fields p

instance
    (KnownSymbol name) =>
    GNamedRecordFields (M1 S ('MetaSel ('Just name) unpackedness strictness decidedStrictness) (K1 R Text))
    where
    genericNamedRecordFields = M1 (K1 (Text.pack (symbolVal @name Proxy)))

instance
    (GNamedRecordFields left, GNamedRecordFields right) =>
    GNamedRecordFields (left :*: right)
    where
    genericNamedRecordFields = genericNamedRecordFields @left :*: genericNamedRecordFields @right

instance GNamedRecordFields U1 where
    genericNamedRecordFields = U1

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
