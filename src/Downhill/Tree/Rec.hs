{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Rec (
    Rec,
    RecIndex,
    HList (HNil, HCons),
    recFieldBuilder,
    recLookup,
    recIndexes,
    fillRec,
    fillRecAt,
    foldMapRec,
    foldMapRecAt,
    foldZipRec,
    foldZipRecAt,
    mapRec,
    mapRecAt,
    zipRec,
    zipRecAt,
    traverseRec,
    traverseRecAt,
    traverseRec_,
    traverseRecAt_,
    recFromElements,
    recElements,
)
where

import Control.Monad.ST (ST, runST)
import Data.Constraint (Dict (Dict))
import Data.DList (DList)
import Data.DList qualified as DList
import Data.HList.HList (HList (HCons, HNil))
import Data.List.Singletons (SList (..))
import Data.Primitive.SmallArray (SmallArray, SmallMutableArray, indexSmallArray, newSmallArray, unsafeFreezeSmallArray, writeSmallArray)
import Data.Proxy (Proxy (Proxy))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Singletons (Apply, Sing, SingI (sing), TyCon1, withSingI, type (@@), type (~>))
import Downhill.Linear.Builder (HasBuilder (..), maybeToMonoid)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.Tree.At (
    At (..),
 )
import Downhill.Tree.Representation (AllConstrained (allConstrained), MultiplicativeActionOver, PointwiseManifold (..), PointwiseVSpace (..), ReprDualOf, ReprScalar, ReprTang)
import Downhill.VSpace (VSpace (..))
import NumHask.Prelude
import Prelude.Singletons (Map)
import Unsafe.Coerce (unsafeCoerce)

newtype Rec (f :: k ~> Type) (fields :: [k]) = Rec (SmallArray Any)

instance (SingI fields, AllConstrained Eq f) => Eq (Rec f fields) where
    x == y = getAll (foldZipRec (sing @fields) eqField x y)
      where
        eqField :: forall field. Sing field -> f @@ field -> f @@ field -> All
        eqField field x' y' =
            case allConstrained @Eq @f field of
                Dict -> All (x' == y')

instance (SingI fields, AllConstrained Show f) => Show (Rec f fields) where
    showsPrec p rec =
        showParen (p > 10)
            $ showString "Rec "
            . showShowSList fieldShows
      where
        fieldShows :: [ShowS]
        fieldShows = foldMapRec (sing @fields) showField rec

        showField :: forall field. Sing field -> f @@ field -> [ShowS]
        showField field x =
            case allConstrained @Show @f field of
                Dict -> [shows x]

        showShowSList :: [ShowS] -> ShowS
        showShowSList xs = showChar '[' . go xs . showChar ']'

        go :: [ShowS] -> ShowS
        go [] = id
        go [x] = x
        go (x : xs) = x . showChar ',' . go xs

instance (SingI fields, AllConstrained Additive f) => Additive (Rec f fields) where
    zero = fillRec (sing @fields) zeroField
      where
        zeroField :: forall field. Sing field -> f @@ field
        zeroField field =
            case allConstrained @Additive @f field of
                Dict -> zero

    x + y = zipRec (sing @fields) addField x y
      where
        addField :: forall field. Sing field -> f @@ field -> f @@ field -> f @@ field
        addField field x' y' =
            case allConstrained @Additive @f field of
                Dict -> x' + y'

instance (SingI fields, AllConstrained Additive f, AllConstrained Subtractive f) => Subtractive (Rec f fields) where
    negate = mapRec (sing @fields) negateField
      where
        negateField :: forall field. Sing field -> f @@ field -> f @@ field
        negateField field x =
            case allConstrained @Subtractive @f field of
                Dict -> negate x

    x - y = zipRec (sing @fields) subtractField x y
      where
        subtractField :: forall field. Sing field -> f @@ field -> f @@ field -> f @@ field
        subtractField field x' y' =
            case allConstrained @Subtractive @f field of
                Dict -> x' - y'

instance (SingI fields, AllConstrained Multiplicative f) => Multiplicative (Rec f fields) where
    one = fillRec (sing @fields) oneField
      where
        oneField :: forall field. Sing field -> f @@ field
        oneField field =
            case allConstrained @Multiplicative @f field of
                Dict -> one

    x * y = zipRec (sing @fields) multiplyField x y
      where
        multiplyField :: forall field. Sing field -> f @@ field -> f @@ field -> f @@ field
        multiplyField field x' y' =
            case allConstrained @Multiplicative @f field of
                Dict -> x' * y'

instance
    ( SingI fields
    , Multiplicative (ReprScalar f)
    , FromInteger (ReprScalar f)
    , AllConstrained (MultiplicativeActionOver (ReprScalar f)) f
    ) =>
    MultiplicativeAction (Rec f fields)
    where
    type Scalar (Rec f fields) = ReprScalar f
    v |* a = mapRec (sing @fields) scaleField v
      where
        scaleField :: forall field. Sing field -> f @@ field -> f @@ field
        scaleField field x =
            case allConstrained @(MultiplicativeActionOver (ReprScalar f)) @f field of
                Dict -> x |* a

type instance DualOf (Rec f fields) = Rec (ReprDualOf f) fields

instance
    ( SingI fields
    , PointwiseVSpace f
    , AllConstrained Additive f
    , AllConstrained Subtractive f
    , AllConstrained Additive (ReprDualOf f)
    , AllConstrained Subtractive (ReprDualOf f)
    , AllConstrained (MultiplicativeActionOver (ReprScalar f)) f
    , AllConstrained (MultiplicativeActionOver (ReprScalar f)) (ReprDualOf f)
    , ReprScalar (ReprDualOf f) ~ ReprScalar f
    , DualOf (ReprScalar f) ~ ReprScalar f
    , ReprDualOf (ReprDualOf f) ~ f
    , Scalar (ReprScalar f) ~ ReprScalar f
    , FromInteger (ReprScalar f)
    , Ring (ReprScalar f)
    , Multiplicative (ReprScalar f)
    ) =>
    VSpace (Rec f fields)
    where
    dv |*| v = getSum (foldZipRec (sing @fields) multiplyField dv v)
      where
        multiplyField :: forall field. Sing field -> ReprDualOf f @@ field -> f @@ field -> Sum (ReprScalar f)
        multiplyField field dv' v' =
            case vspaceDict @f field of
                Dict -> Sum (dv' |*| v')

type instance Tang (Rec f fields) = Rec (ReprTang f) fields

instance
    ( SingI fields
    , PointwiseManifold f
    , PointwiseVSpace (ReprTang f)
    , ReprScalar (ReprDualOf (ReprTang f)) ~ ReprScalar (ReprTang f)
    , DualOf (ReprScalar (ReprTang f)) ~ ReprScalar (ReprTang f)
    , ReprDualOf (ReprDualOf (ReprTang f)) ~ ReprTang f
    , ReprTang (ReprDualOf (ReprTang f)) ~ ReprDualOf (ReprTang f)
    , ReprTang (ReprTang f) ~ ReprTang f
    , Tang (ReprScalar (ReprTang f)) ~ ReprScalar (ReprTang f)
    , Scalar (ReprScalar (ReprTang f)) ~ ReprScalar (ReprTang f)
    , FromInteger (ReprScalar (ReprTang f))
    , Ring (ReprScalar (ReprTang f))
    , Multiplicative (ReprScalar (ReprTang f))
    , AllConstrained Additive (ReprTang f)
    , AllConstrained Subtractive (ReprTang f)
    , AllConstrained Additive (ReprDualOf (ReprTang f))
    , AllConstrained Subtractive (ReprDualOf (ReprTang f))
    , AllConstrained (MultiplicativeActionOver (ReprScalar (ReprTang f))) (ReprTang f)
    , AllConstrained (MultiplicativeActionOver (ReprScalar (ReprTang f))) (ReprDualOf (ReprTang f))
    ) =>
    Manifold (Rec f fields)
    where
    x .+~^ dx = zipRec (sing @fields) addTangentField x dx
      where
        addTangentField :: forall field. Sing field -> f @@ field -> ReprTang f @@ field -> f @@ field
        addTangentField field x' dx' =
            case manifoldDict @f field of
                Dict -> x' .+~^ dx'

    x .-~^ dx = zipRec (sing @fields) subtractTangentField x dx
      where
        subtractTangentField :: forall field. Sing field -> f @@ field -> ReprTang f @@ field -> f @@ field
        subtractTangentField field x' dx' =
            case manifoldDict @f field of
                Dict -> x' .-~^ dx'

data RecFieldBuilder f fields = forall field. RecFieldBuilder (RecIndex fields field) (VecBuilder (f @@ field))

recFieldBuilder :: RecIndex fields field -> VecBuilder (f @@ field) -> VecBuilder (Rec f fields)
recFieldBuilder index builder = RecBuilder Nothing (DList.singleton (RecFieldBuilder index builder))

data RecBuilder f fields = RecBuilder (Maybe (Sum (Rec f fields))) (DList (RecFieldBuilder f fields))

instance (Additive (Rec f fields)) => Semigroup (RecBuilder f fields) where
    RecBuilder x1 y1 <> RecBuilder x2 y2 = RecBuilder (x1 <> x2) (y1 <> y2)

instance (Additive (Rec f fields)) => Monoid (RecBuilder f fields) where
    mempty = RecBuilder mempty mempty

data FieldAccum s f field where
    FieldAccum :: (HasBuilder (f @@ field)) => STRef s (VecBuilder (f @@ field)) -> FieldAccum s f field

data RecAccum s (f :: k ~> Type) :: k ~> Type
type instance Apply (RecAccum s f) field = FieldAccum s f field

sumRecBuilder ::
    forall {k} (f :: k ~> Type) (fields :: [k]).
    (SingI fields, AllConstrained Additive f, AllConstrained HasBuilder f) =>
    RecBuilder f fields ->
    Rec f fields
sumRecBuilder (RecBuilder densePart fields) = case DList.toList fields of
    [] -> denseRecord
    sparseFields -> runST (go sparseFields)
  where
    denseRecord :: Rec f fields
    denseRecord = getSum (maybeToMonoid densePart)

    initField :: forall field s. Sing field -> f @@ field -> ST s (RecAccum s f @@ field)
    initField field x =
        case allConstrained @HasBuilder @f field of
            Dict -> FieldAccum <$> newSTRef (identityBuilder x)

    bumpField :: forall s. Rec (RecAccum s f) fields -> RecFieldBuilder f fields -> ST s ()
    bumpField accum (RecFieldBuilder index builder) =
        case recLookup index accum of
            FieldAccum ref -> modifySTRef ref (<> builder)

    sumField :: forall field s. Sing field -> RecAccum s f @@ field -> ST s (f @@ field)
    sumField _ (FieldAccum ref) = sumBuilder <$> readSTRef ref

    go :: forall s. [RecFieldBuilder f fields] -> ST s (Rec f fields)
    go sparseFields = do
        accum <- traverseRec (sing @fields) initField denseRecord
        traverse_ (bumpField accum) sparseFields
        traverseRec (sing @fields) sumField accum

instance
    ( SingI fields
    , AllConstrained Additive f
    , AllConstrained HasBuilder f
    ) =>
    HasBuilder (Rec f fields)
    where
    type VecBuilder (Rec f fields) = RecBuilder f fields
    sumBuilder = sumRecBuilder
    identityBuilder x = RecBuilder (Just (Sum x)) mempty

recLookup :: forall {k} (f :: k ~> Type) (fields :: [k]) (field :: k). RecIndex fields field -> Rec f fields -> f @@ field
recLookup (RecIndex i) rec = indexRec @f @field rec i

recIndexes :: forall {k} (fields :: [k]). Sing fields -> Rec (TyCon1 (RecIndex fields)) fields
recIndexes shapes =
    Rec $ runST $ do
        xs <- newSmallArray (singLength shapes) (unsafeCoerce ())
        writeIndexes (Proxy @fields) 0 shapes xs
        unsafeFreezeSmallArray xs

fillRec :: forall {k} (f :: k ~> Type) (fields :: [k]). Sing fields -> (forall field. Sing field -> f @@ field) -> Rec f fields
fillRec shapes f = buildRec shapes (\_ field -> f field)

fillRecAt ::
    forall {k} (f :: k ~> Type) (fields :: [k]).
    Sing fields ->
    (forall field. (SingI field) => At f field) ->
    Rec f fields
fillRecAt shapes f = fillRec shapes fillField
  where
    fillField :: forall field. Sing field -> f @@ field
    fillField field = withSingI field (unAt (f @field))

foldMapRec ::
    forall {k} (f :: k ~> Type) (fields :: [k]) m.
    (Monoid m) =>
    Sing fields ->
    (forall field. Sing field -> f @@ field -> m) ->
    Rec f fields ->
    m
foldMapRec shapes f rec = go 0 shapes
  where
    go :: Int -> Sing (fields' :: [k]) -> m
    go _ SNil = mempty
    go i (SCons (field :: Sing field) fields) =
        f field (indexRec @f @field rec i) <> go (i + 1) fields

foldMapRecAt ::
    forall {k} (f :: k ~> Type) (fields :: [k]) m.
    (Monoid m) =>
    Sing fields ->
    (forall field. (SingI field) => At f field -> m) ->
    Rec f fields ->
    m
foldMapRecAt shapes f = foldMapRec shapes foldField
  where
    foldField :: forall field. Sing field -> f @@ field -> m
    foldField field x = withSingI field (f @field (At x))

foldZipRec ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (fields :: [k]) m.
    (Monoid m) =>
    Sing fields ->
    (forall field. Sing field -> a @@ field -> b @@ field -> m) ->
    Rec a fields ->
    Rec b fields ->
    m
foldZipRec shapes f recA recB = go 0 shapes
  where
    go :: Int -> Sing (fields' :: [k]) -> m
    go _ SNil = mempty
    go i (SCons (field :: Sing field) fields) =
        f field (indexRec @a @field recA i) (indexRec @b @field recB i) <> go (i + 1) fields

foldZipRecAt ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (fields :: [k]) m.
    (Monoid m) =>
    Sing fields ->
    (forall field. (SingI field) => At a field -> At b field -> m) ->
    Rec a fields ->
    Rec b fields ->
    m
foldZipRecAt shapes f = foldZipRec shapes foldField
  where
    foldField :: forall field. Sing field -> a @@ field -> b @@ field -> m
    foldField field x y = withSingI field (f @field (At x) (At y))

mapRec ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (fields :: [k]).
    Sing fields ->
    (forall field. Sing field -> a @@ field -> b @@ field) ->
    Rec a fields ->
    Rec b fields
mapRec shapes f rec = buildRec shapes (\i (field :: Sing field) -> f field (indexRec @a @field rec i))

mapRecAt ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (fields :: [k]).
    Sing fields ->
    (forall field. (SingI field) => At a field -> At b field) ->
    Rec a fields ->
    Rec b fields
mapRecAt shapes f = mapRec shapes mapField
  where
    mapField :: forall field. Sing field -> a @@ field -> b @@ field
    mapField field x = withSingI field (unAt (f @field (At x)))

zipRec ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (c :: k ~> Type) (fields :: [k]).
    Sing fields ->
    (forall field. Sing field -> a @@ field -> b @@ field -> c @@ field) ->
    Rec a fields ->
    Rec b fields ->
    Rec c fields
zipRec shapes f recA recB =
    buildRec shapes (\i (field :: Sing field) -> f field (indexRec @a @field recA i) (indexRec @b @field recB i))

zipRecAt ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (c :: k ~> Type) (fields :: [k]).
    Sing fields ->
    (forall field. (SingI field) => At a field -> At b field -> At c field) ->
    Rec a fields ->
    Rec b fields ->
    Rec c fields
zipRecAt shapes f = zipRec shapes zipField
  where
    zipField :: forall field. Sing field -> a @@ field -> b @@ field -> c @@ field
    zipField field x y = withSingI field (unAt (f @field (At x) (At y)))

traverseRec ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (fields :: [k]) m.
    (Applicative m) =>
    Sing fields ->
    (forall field. Sing field -> a @@ field -> m (b @@ field)) ->
    Rec a fields ->
    m (Rec b fields)
traverseRec shapes f rec = recFromElements shapes <$> go 0 shapes
  where
    go :: Int -> Sing fields' -> m (HList (Map b fields'))
    go _ SNil = pure HNil
    go i (SCons (field :: Sing field) fields) = HCons <$> f field (indexRec @a @field rec i) <*> go (i + 1) fields

traverseRecAt ::
    forall {k} (a :: k ~> Type) (b :: k ~> Type) (fields :: [k]) m.
    (Applicative m) =>
    Sing fields ->
    (forall field. (SingI field) => At a field -> m (At b field)) ->
    Rec a fields ->
    m (Rec b fields)
traverseRecAt shapes f = traverseRec shapes traverseField
  where
    traverseField :: forall field. Sing field -> a @@ field -> m (b @@ field)
    traverseField field x = unAt <$> withSingI field (f @field (At x))

traverseRec_ ::
    forall {k} (f :: k ~> Type) (fields :: [k]) m.
    (Applicative m) =>
    Sing fields ->
    (forall field. Sing field -> f @@ field -> m ()) ->
    Rec f fields ->
    m ()
traverseRec_ shapes f rec = go 0 shapes
  where
    go :: Int -> Sing (fields' :: [k]) -> m ()
    go _ SNil = pure ()
    go i (SCons (field :: Sing field) fields) = f field (indexRec @f @field rec i) *> go (i + 1) fields

traverseRecAt_ ::
    forall {k} (f :: k ~> Type) (fields :: [k]) m.
    (Applicative m) =>
    Sing fields ->
    (forall field. (SingI field) => At f field -> m ()) ->
    Rec f fields ->
    m ()
traverseRecAt_ shapes f = traverseRec_ shapes traverseField
  where
    traverseField :: forall field. Sing field -> f @@ field -> m ()
    traverseField field x = withSingI field (f @field (At x))

recFromElements :: forall {k} (f :: k ~> Type) (fields :: [k]). Sing fields -> HList (Map f fields) -> Rec f fields
recFromElements shapes fields =
    Rec $ runST $ do
        xs <- newSmallArray (singLength shapes) (unsafeCoerce ())
        writeHList @f 0 shapes fields xs
        unsafeFreezeSmallArray xs

recElements :: forall {k} (f :: k ~> Type) (fields :: [k]). Sing fields -> Rec f fields -> HList (Map f fields)
recElements fields rec = go 0 fields
  where
    go :: Int -> Sing fields' -> HList (Map f fields')
    go _ SNil = HNil
    go i (SCons (_ :: Sing field) fields') = HCons (indexRec @f @field rec i) (go (i + 1) fields')

indexRec :: forall {k} (f :: k ~> Type) (field :: k) fields. Rec f fields -> Int -> f @@ field
indexRec (Rec xs) i = unsafeCoerce (indexSmallArray xs i)

buildRec ::
    forall {k} (f :: k ~> Type) (fields :: [k]).
    Sing fields ->
    (forall field. Int -> Sing field -> f @@ field) ->
    Rec f fields
buildRec shapes f =
    Rec $ runST $ do
        xs <- newSmallArray (singLength shapes) (unsafeCoerce ())
        writeBuild @f 0 shapes f xs
        unsafeFreezeSmallArray xs

singLength :: forall {k} (fields :: [k]). Sing fields -> Int
singLength SNil = 0
singLength (SCons _ fields) = 1 + singLength fields

writeHList ::
    forall {k} (f :: k ~> Type) (fields :: [k]) s.
    Int ->
    Sing fields ->
    HList (Map f fields) ->
    SmallMutableArray s Any ->
    ST s ()
writeHList _ SNil HNil _ = pure ()
writeHList i (SCons _ shapes) (HCons field fields) xs = do
    writeSmallArray xs i (unsafeCoerce field)
    writeHList @f (i + 1) shapes fields xs

writeBuild ::
    forall {k} (f :: k ~> Type) (fields :: [k]) s.
    Int ->
    Sing fields ->
    (forall field. Int -> Sing field -> f @@ field) ->
    SmallMutableArray s Any ->
    ST s ()
writeBuild _ SNil _ _ = pure ()
writeBuild i (SCons (field :: Sing field) shapes) f xs = do
    writeSmallArray xs i (unsafeCoerce (f i field))
    writeBuild @f (i + 1) shapes f xs

writeIndexes ::
    forall {k} (allFields :: [k]) (fields :: [k]) s.
    Proxy allFields ->
    Int ->
    Sing fields ->
    SmallMutableArray s Any ->
    ST s ()
writeIndexes _ _ SNil _ = pure ()
writeIndexes allFields i (SCons (_ :: Sing field) shapes) xs = do
    let ix :: RecIndex allFields field
        ix = RecIndex i
    writeSmallArray xs i (unsafeCoerce ix)
    writeIndexes allFields (i + 1) shapes xs

newtype RecIndex (fields :: [k]) (field :: k) = RecIndex Int
