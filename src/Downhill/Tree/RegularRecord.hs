{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.RegularRecord (
    -- * Shapes
    RegularTreeShape (..),
    SRegularTreeShape (..),
    FieldNaming (..),
    RecordFieldNames (..),

    -- * Records
    IsRegularRecord (..),
    fillRecord,
    mapRecord,
    traverseRecord,
    zipRecord,
    foldMapRecord,
    foldZipRecord,

    -- * Trees
    RegularTreeRep,
    RecordTree (..),
    fillTree,
    mapTree,
    traverseTree,
    zipTree,
    foldMapTree,
    foldZipTree,
    fillTreeI,
    mapTreeI,
    traverseTreeI,
    zipTreeI,

    -- * Automatic differentiation
    adRecordTree,
)
where

import Data.Constraint (Dict (..))
import Data.Singletons (Apply, Proxy (..), Sing, SingI (..), TyCon1, withSingI, type (@@), type (~>))
import Data.Text qualified
import Downhill.BVar (BVar (..), DFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.AD (inlineNode, projectNode)
import Downhill.Linear.Builder (DenseSparseBuilder, HasBuilder (..), denseBuilder, flattenDenseSparseBuilder, sparseBuilder, sumDenseSparseBuilder)
import Downhill.Linear.Fun (AsLinearMap (..), DualOf, LinearMap (..), Metric)
import Downhill.Manifold (Grad, Manifold (..), Tang)
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.FieldNaming (FieldNaming (..))
import Downhill.Tree.HListOf (HListBuilder (..), HListOf (..), fillHListOf, foldMapHListOf, foldZipHListOf, joinBVarHList, mapHListOf, splitBVarHList, traverseHListOf, zipHListOf, pattern HConsOf, pattern HNilOf)
import Downhill.Tree.Representation (AdditiveRepr, AllConstrained (..), BuilderRepr, HasGradRepr, InnerSpaceRepr, ManifoldRepr, MultiplicativeActionOver, MultiplicativeActionRepr, PointwiseManifold (manifoldDict), PointwiseMetric (metricDict), PointwiseVSpace (vspaceDict), ReprDualOf, ReprGrad, ReprScalar, ReprTang, SubtractiveRepr, VSpaceRepr, innerSpaceReprDict, manifoldReprDict)
import Downhill.VSpace (InnerSpace (..), VSpace (..))
import GHC.TypeLits (ErrorMessage (Text), KnownSymbol, TypeError, symbolVal)
import NumHask.Prelude
import Prelude.Singletons (ConstSym1)

data RegularTreeShape k = RegularTreeShapeLeaf k | RegularTreeShapeSubtree ((k ~> Type) -> Type)

data SRegularTreeShape (sh :: RegularTreeShape k) where
    SRegularTreeShapeLeaf :: forall sh. Sing sh -> SRegularTreeShape ('RegularTreeShapeLeaf sh)
    SRegularTreeShapeSubtree ::
        forall record.
        (IsRegularRecord record) =>
        SRegularTreeShape ('RegularTreeShapeSubtree record)

type instance Sing = SRegularTreeShape

instance (SingI sh) => SingI ('RegularTreeShapeLeaf sh) where
    sing = SRegularTreeShapeLeaf sing

instance (IsRegularRecord record) => SingI ('RegularTreeShapeSubtree record) where
    sing = SRegularTreeShapeSubtree

data RegularTreeRep (leaf :: k ~> Type) :: RegularTreeShape k ~> Type
type instance Apply (RegularTreeRep leaf) ('RegularTreeShapeLeaf sh) = leaf @@ sh
type instance Apply (RegularTreeRep leaf) ('RegularTreeShapeSubtree record) = record leaf

data RecordFieldNames (naming :: FieldNaming) (fields :: [RegularTreeShape k]) where
    UnnamedFields :: RecordFieldNames 'Unnamed fields
    NamedFields :: HListOf (ConstSym1 Data.Text.Text) fields -> RecordFieldNames 'Named fields

type IsRegularRecord :: forall {leafSh}. ((leafSh ~> Type) -> Type) -> Constraint
class (SingI (FieldShapes record)) => IsRegularRecord (record :: (leafSh ~> Type) -> Type) where
    -- |  A place for free form additional information for user to hook any customizations
    type RegularInfo record :: Type

    type RegularInfo record = ()

    type FieldNamingOf record :: FieldNaming
    type FieldNamingOf record = GenericFieldNaming record

    type FieldShapes record :: [RegularTreeShape leafSh]
    type FieldShapes record = GenericFieldShapes record

    toRegularRecordFields :: record leaf -> HListOf (RegularTreeRep leaf) (FieldShapes record)
    default toRegularRecordFields ::
        forall leaf.
        ( Generic (record ProxyLeaf)
        , Generic (record leaf)
        , GenericRegularRecord record (Rep (record ProxyLeaf)) (Rep (record leaf)) leaf
        ) =>
        record leaf ->
        HListOf (RegularTreeRep leaf) (FieldShapes record)
    toRegularRecordFields record = genericToRegularFieldsRep @_ @record @(Rep (record ProxyLeaf)) @_ @leaf @_ (from record)

    fromRegularRecordFields :: HListOf (RegularTreeRep leaf) (FieldShapes record) -> record leaf
    default fromRegularRecordFields ::
        forall leaf.
        ( Generic (record ProxyLeaf)
        , Generic (record leaf)
        , GenericRegularRecord record (Rep (record ProxyLeaf)) (Rep (record leaf)) leaf
        ) =>
        HListOf (RegularTreeRep leaf) (FieldShapes record) ->
        record leaf
    fromRegularRecordFields record = to $ genericFromRegularFieldsRep @_ @record @(Rep (record ProxyLeaf)) @_ @leaf record

    fieldNames :: RecordFieldNames (FieldNamingOf record) (FieldShapes record)
    default fieldNames ::
        (GenericRegularFieldNames (FieldNamingOf record) (Rep (record ProxyLeaf)) (FieldShapes record)) =>
        RecordFieldNames (FieldNamingOf record) (FieldShapes record)
    fieldNames = genericFieldNames @(FieldNamingOf record) @(Rep (record ProxyLeaf))

fillRecord ::
    forall {k} record (leaf :: k ~> Type).
    (IsRegularRecord record) =>
    (forall (sh :: RegularTreeShape k). Sing sh -> RegularTreeRep leaf @@ sh) ->
    record leaf
fillRecord fieldValue = fromRegularRecordFields $ fillHListOf sing fieldValue

fillTree ::
    forall {k} (leaf :: k ~> Type) (sh :: RegularTreeShape k).
    () =>
    (forall (sh' :: k). Sing sh' -> leaf @@ sh') ->
    Sing sh ->
    RegularTreeRep leaf @@ sh
fillTree leafValue = go
  where
    go :: forall sh'. Sing sh' -> RegularTreeRep leaf @@ sh'
    go (SRegularTreeShapeLeaf sh') = leafValue sh'
    go SRegularTreeShapeSubtree = fillRecord go

mapRecord ::
    forall {k} record (leafA :: k ~> Type) (leafB :: k ~> Type).
    (IsRegularRecord record) =>
    (forall (sh :: RegularTreeShape k). Sing sh -> RegularTreeRep leafA @@ sh -> RegularTreeRep leafB @@ sh) ->
    record leafA ->
    record leafB
mapRecord fieldF = fromRegularRecordFields . mapHListOf fieldF sing . toRegularRecordFields

traverseRecord ::
    forall {k} record (leafA :: k ~> Type) (leafB :: k ~> Type) m.
    (IsRegularRecord record, Applicative m) =>
    (forall (sh :: RegularTreeShape k). Sing sh -> RegularTreeRep leafA @@ sh -> m (RegularTreeRep leafB @@ sh)) ->
    record leafA ->
    m (record leafB)
traverseRecord fieldF = fmap fromRegularRecordFields . traverseHListOf fieldF sing . toRegularRecordFields

zipRecord ::
    forall {k} record (leafA :: k ~> Type) (leafB :: k ~> Type) (leafC :: k ~> Type).
    (IsRegularRecord record) =>
    ( forall (sh :: RegularTreeShape k).
      Sing sh ->
      RegularTreeRep leafA @@ sh ->
      RegularTreeRep leafB @@ sh ->
      RegularTreeRep leafC @@ sh
    ) ->
    record leafA ->
    record leafB ->
    record leafC
zipRecord fieldF rx ry = fromRegularRecordFields $ zipHListOf fieldF sing (toRegularRecordFields rx) (toRegularRecordFields ry)

foldMapRecord ::
    forall {k} record (leafA :: k ~> Type) m.
    (IsRegularRecord record, Monoid m) =>
    (forall (sh :: RegularTreeShape k). Sing sh -> RegularTreeRep leafA @@ sh -> m) ->
    record leafA ->
    m
foldMapRecord f = foldMapHListOf f sing . toRegularRecordFields

foldZipRecord ::
    forall {k} record (leafA :: k ~> Type) (leafB :: k ~> Type) m.
    (IsRegularRecord record, Monoid m) =>
    (forall (sh :: RegularTreeShape k). Sing sh -> RegularTreeRep leafA @@ sh -> RegularTreeRep leafB @@ sh -> m) ->
    record leafA ->
    record leafB ->
    m
foldZipRecord fieldF rx ry = foldZipHListOf fieldF sing (toRegularRecordFields rx) (toRegularRecordFields ry)

mapTree ::
    forall {k} (leafA :: k ~> Type) (leafB :: k ~> Type) (sh :: RegularTreeShape k).
    () =>
    (forall (sh' :: k). Sing sh' -> leafA @@ sh' -> leafB @@ sh') ->
    Sing sh ->
    RegularTreeRep leafA @@ sh ->
    RegularTreeRep leafB @@ sh
mapTree f = go
  where
    go :: forall sh'. Sing sh' -> RegularTreeRep leafA @@ sh' -> RegularTreeRep leafB @@ sh'
    go (SRegularTreeShapeLeaf sh') = f sh'
    go SRegularTreeShapeSubtree = mapRecord go

traverseTree ::
    forall {k} (leafA :: k ~> Type) (leafB :: k ~> Type) m (sh :: RegularTreeShape k).
    (Applicative m) =>
    (forall (sh' :: k). Sing sh' -> leafA @@ sh' -> m (leafB @@ sh')) ->
    Sing sh ->
    RegularTreeRep leafA @@ sh ->
    m (RegularTreeRep leafB @@ sh)
traverseTree f = go
  where
    go :: forall sh'. Sing sh' -> RegularTreeRep leafA @@ sh' -> m (RegularTreeRep leafB @@ sh')
    go (SRegularTreeShapeLeaf sh') = f sh'
    go SRegularTreeShapeSubtree = traverseRecord go

zipTree ::
    forall {k} (leafA :: k ~> Type) (leafB :: k ~> Type) (leafC :: k ~> Type) (sh :: RegularTreeShape k).
    () =>
    (forall (sh' :: k). Sing sh' -> leafA @@ sh' -> leafB @@ sh' -> leafC @@ sh') ->
    Sing sh ->
    RegularTreeRep leafA @@ sh ->
    RegularTreeRep leafB @@ sh ->
    RegularTreeRep leafC @@ sh
zipTree f = go
  where
    go :: forall sh'. Sing sh' -> RegularTreeRep leafA @@ sh' -> RegularTreeRep leafB @@ sh' -> RegularTreeRep leafC @@ sh'
    go (SRegularTreeShapeLeaf sh') = f sh'
    go SRegularTreeShapeSubtree = zipRecord go

foldMapTree ::
    forall {k} (leafA :: k ~> Type) m (sh :: RegularTreeShape k).
    (Monoid m) =>
    (forall (sh' :: k). Sing sh' -> leafA @@ sh' -> m) ->
    Sing sh ->
    RegularTreeRep leafA @@ sh ->
    m
foldMapTree f = go
  where
    go :: forall sh'. Sing sh' -> RegularTreeRep leafA @@ sh' -> m
    go (SRegularTreeShapeLeaf sh') = f sh'
    go SRegularTreeShapeSubtree = foldMapRecord go

foldZipTree ::
    forall {k} (leafA :: k ~> Type) (leafB :: k ~> Type) m (sh :: RegularTreeShape k).
    (Monoid m) =>
    (forall (sh' :: k). Sing sh' -> leafA @@ sh' -> leafB @@ sh' -> m) ->
    Sing sh ->
    RegularTreeRep leafA @@ sh ->
    RegularTreeRep leafB @@ sh ->
    m
foldZipTree f = go
  where
    go :: forall sh'. Sing sh' -> RegularTreeRep leafA @@ sh' -> RegularTreeRep leafB @@ sh' -> m
    go (SRegularTreeShapeLeaf sh') = f sh'
    go SRegularTreeShapeSubtree = foldZipRecord go

newtype RecordTree leaf sh = RecordTree {unRecordTree :: RegularTreeRep leaf @@ sh}

fillTreeI :: forall c leaf sh. (AllConstrained c leaf, SingI sh) => (forall a. ((c a) => a)) -> RecordTree leaf sh
fillTreeI x = RecordTree (fillTree @leaf @sh x' sing)
  where
    x' :: forall sh'. Sing sh' -> leaf @@ sh'
    x' sh = case allConstrained @c @leaf sh of
        Dict -> x

mapTreeI :: forall c leaf sh. (AllConstrained c leaf, SingI sh) => (forall a. ((c a) => a -> a)) -> RecordTree leaf sh -> RecordTree leaf sh
mapTreeI f (RecordTree x) = RecordTree (mapTree @leaf @leaf @sh f' sing x)
  where
    f' :: forall sh'. Sing sh' -> leaf @@ sh' -> leaf @@ sh'
    f' sh x' = case allConstrained @c @leaf sh of
        Dict -> f x'

traverseTreeI ::
    forall m c leaf sh.
    (Applicative m, SingI sh, AllConstrained c leaf) =>
    (forall a. (c a) => a -> m a) ->
    RecordTree leaf sh ->
    m (RecordTree leaf sh)
traverseTreeI f (RecordTree x) =
    RecordTree <$> traverseTree @leaf @leaf @m @sh f' sing x
  where
    f' :: Sing sh' -> leaf @@ sh' -> m (leaf @@ sh')
    f' sh x' = case allConstrained @c @leaf sh of
        Dict -> f x'

zipTreeI :: forall c leaf sh. (AllConstrained c leaf, SingI sh) => (forall a. ((c a) => a -> a -> a)) -> RecordTree leaf sh -> RecordTree leaf sh -> RecordTree leaf sh
zipTreeI f (RecordTree x) (RecordTree y) = RecordTree (zipTree @leaf @leaf @leaf @sh f' sing x y)
  where
    f' :: forall sh'. Sing sh' -> leaf @@ sh' -> leaf @@ sh' -> leaf @@ sh'
    f' sh x' = case allConstrained @c @leaf sh of
        Dict -> f x'

type instance DualOf (RecordTree leaf sh) = RecordTree (ReprDualOf leaf) sh
instance (VSpaceRepr leaf, SingI sh) => VSpace (RecordTree leaf sh) where
    RecordTree dx |*| RecordTree x = getSum $ foldZipTree @(ReprDualOf leaf) @leaf @(Sum (ReprScalar leaf)) @sh goLeaf sing dx x
      where
        goLeaf :: Sing sh' -> ReprDualOf leaf @@ sh' -> leaf @@ sh' -> Sum (ReprScalar leaf)
        goLeaf sh = case vspaceDict @leaf sh of
            Dict -> \dx' x' -> Sum (dx' |*| x')

instance
    ( PointwiseMetric metric cov
    , VSpaceRepr cov
    , vec ~ ReprDualOf cov
    , cov ~ ReprDualOf vec
    , SingI sh
    ) =>
    AsLinearMap (RecordTree metric sh) (RecordTree cov sh) (RecordTree vec sh)
    where
    toLinearMap (RecordTree metrics) = LinearMap fwd back
      where
        fwd (RecordTree x) = RecordTree $ zipTree @metric @cov @vec @sh fwdLeaf sing metrics x
        back (RecordTree x) = RecordTree $ zipTree @metric @cov @vec @sh backLeaf sing metrics x

        fwdLeaf :: Sing sh' -> metric @@ sh' -> cov @@ sh' -> vec @@ sh'
        fwdLeaf sh m x = case metricDict @metric @cov sh of
            Dict -> case vspaceDict @cov sh of
                Dict -> lmapFwd (toLinearMap m) x

        backLeaf :: forall sh'. Sing sh' -> metric @@ sh' -> cov @@ sh' -> vec @@ sh'
        backLeaf sh m x = case metricDict @metric @cov sh of
            Dict -> case vspaceDict @cov sh of
                Dict -> lmapBack (toLinearMap @(metric @@ sh') @(cov @@ sh') m) x

instance
    (PointwiseMetric metric cov, VSpaceRepr cov, SingI sh) =>
    Metric (RecordTree metric sh) (RecordTree cov sh)

instance (InnerSpaceRepr leaf, SingI sh) => InnerSpace (RecordTree leaf sh) where
    riesz (RecordTree x) = RecordTree $ mapTree @leaf @(ReprDualOf leaf) @sh goLeaf sing x
      where
        goLeaf :: Sing sh' -> leaf @@ sh' -> ReprDualOf leaf @@ sh'
        goLeaf sh x' = case innerSpaceReprDict @leaf sh of
            Dict -> riesz x'

    coriesz (RecordTree dx) = RecordTree $ mapTree @(ReprDualOf leaf) @leaf @sh goLeaf sing dx
      where
        goLeaf :: Sing sh' -> ReprDualOf leaf @@ sh' -> leaf @@ sh'
        goLeaf sh dx' = case innerSpaceReprDict @leaf sh of
            Dict -> coriesz dx'

    RecordTree x <.> RecordTree y = getSum $ foldZipTree @leaf @leaf @(Sum (ReprScalar leaf)) @sh goLeaf sing x y
      where
        goLeaf :: Sing sh' -> leaf @@ sh' -> leaf @@ sh' -> Sum (ReprScalar leaf)
        goLeaf sh x' y' = case innerSpaceReprDict @leaf sh of
            Dict -> Sum (x' <.> y')

type instance Tang (RecordTree leaf sh) = RecordTree (ReprTang leaf) sh
instance (ManifoldRepr leaf, SingI sh) => Manifold (RecordTree leaf sh) where
    RecordTree p .+~^ RecordTree v = RecordTree $ zipTree @leaf @(ReprTang leaf) @leaf @sh go sing p v
      where
        go :: Sing sh' -> leaf @@ sh' -> ReprTang leaf @@ sh' -> leaf @@ sh'
        go sh = case manifoldDict @leaf sh of
            Dict -> (.+~^)
    RecordTree p .-~^ RecordTree v = RecordTree $ zipTree @leaf @(ReprTang leaf) @leaf @sh go sing p v
      where
        go :: Sing sh' -> leaf @@ sh' -> ReprTang leaf @@ sh' -> leaf @@ sh'
        go sh = case manifoldDict @leaf sh of
            Dict -> (.-~^)

type instance ReprScalar (TyCon1 (RecordTree leaf)) = ReprScalar leaf
type instance ReprDualOf (TyCon1 (RecordTree leaf)) = TyCon1 (RecordTree (ReprDualOf leaf))
type instance ReprTang (TyCon1 (RecordTree leaf)) = TyCon1 (RecordTree (ReprTang leaf))

instance (forall sh. (SingI sh) => c (RecordTree leaf sh)) => AllConstrained c (TyCon1 (RecordTree leaf)) where
    allConstrained sh = withSingI sh Dict

instance (VSpaceRepr leaf) => PointwiseVSpace (TyCon1 (RecordTree leaf)) where
    vspaceDict (SRegularTreeShapeLeaf sh) = withSingI sh Dict
    vspaceDict SRegularTreeShapeSubtree = Dict

instance (ManifoldRepr leaf) => PointwiseManifold (TyCon1 (RecordTree leaf)) where
    manifoldDict (SRegularTreeShapeLeaf sh) = withSingI sh Dict
    manifoldDict SRegularTreeShapeSubtree = Dict

instance (AdditiveRepr leaf, SingI sh) => Additive (RecordTree leaf sh) where
    zero = fillTreeI @Additive zero
    (+) = zipTreeI @Additive (+)

instance (SubtractiveRepr leaf, SingI sh) => Subtractive (RecordTree leaf sh) where
    negate = mapTreeI @Subtractive negate
    (-) = zipTreeI @Subtractive (-)

instance (MultiplicativeActionRepr leaf, SingI sh) => MultiplicativeAction (RecordTree leaf sh) where
    type Scalar (RecordTree leaf sh) = ReprScalar leaf
    v |* a = mapTreeI @(MultiplicativeActionOver (ReprScalar leaf)) (|* a) v

wrapRecordTrees ::
    forall {k} (leaf :: k ~> Type) fields.
    Sing fields ->
    HListOf (RegularTreeRep leaf) fields ->
    HListOf (TyCon1 (RecordTree leaf)) fields
wrapRecordTrees = mapHListOf (const RecordTree)

unwrapRecordTrees ::
    forall {k} (leaf :: k ~> Type) fields.
    Sing fields ->
    HListOf (TyCon1 (RecordTree leaf)) fields ->
    HListOf (RegularTreeRep leaf) fields
unwrapRecordTrees = mapHListOf (const unRecordTree)

type family RecordTreeVecBuilderRep (leaf :: k ~> Type) (sh :: RegularTreeShape k) :: Type where
    RecordTreeVecBuilderRep leaf ('RegularTreeShapeLeaf leafSh) = VecBuilder (leaf @@ leafSh)
    RecordTreeVecBuilderRep leaf ('RegularTreeShapeSubtree record) =
        DenseSparseBuilder
            (RecordTree leaf ('RegularTreeShapeSubtree record))
            (HListBuilder (TyCon1 (RecordTree leaf)) (FieldShapes record))

newtype RecordTreeVecBuilder (leaf :: k ~> Type) (sh :: RegularTreeShape k) = RecordTreeVecBuilder {unRecordTreeVecBuilder :: RecordTreeVecBuilderRep leaf sh}

instance (SingI sh, BuilderRepr leaf) => Semigroup (RecordTreeVecBuilder leaf sh) where
    RecordTreeVecBuilder x <> RecordTreeVecBuilder y = RecordTreeVecBuilder $ case sing @sh of
        SRegularTreeShapeLeaf sh -> case allConstrained @HasBuilder @leaf sh of
            Dict -> x <> y
        SRegularTreeShapeSubtree -> x <> y

instance (SingI sh, BuilderRepr leaf) => Monoid (RecordTreeVecBuilder leaf sh) where
    mempty = RecordTreeVecBuilder $ case sing @sh of
        SRegularTreeShapeLeaf sh -> case allConstrained @HasBuilder @leaf sh of
            Dict -> mempty
        SRegularTreeShapeSubtree -> mempty

instance (BuilderRepr leaf, SingI sh) => HasBuilder (RecordTree leaf sh) where
    type VecBuilder (RecordTree leaf sh) = RecordTreeVecBuilder leaf sh
    sumBuilder (RecordTreeVecBuilder x) = case sing @sh of
        SRegularTreeShapeLeaf sh -> case allConstrained @HasBuilder @leaf sh of
            Dict -> RecordTree (sumBuilder x)
        SRegularTreeShapeSubtree ->
            sumDenseSparseBuilder
                (identityBuilder . toRecordTreeFields)
                (fromRecordTreeFields . sumBuilder)
                x

    identityBuilder tree@(RecordTree x) = RecordTreeVecBuilder $ case sing @sh of
        SRegularTreeShapeLeaf sh -> case allConstrained @HasBuilder @leaf sh of
            Dict -> identityBuilder x
        SRegularTreeShapeSubtree -> denseBuilder tree

wrapBVarLeafTree :: forall r leaf sh. (ManifoldRepr leaf) => Sing sh -> BVar r (leaf @@ sh) -> BVar r (RecordTree leaf (RegularTreeShapeLeaf sh))
wrapBVarLeafTree sh (BVar x dx) = case manifoldReprDict @leaf sh of
    Dict -> BVar (RecordTree x) (inlineNode RecordTree unRecordTreeVecBuilder dx)

unwrapBVarLeafTree :: forall r leaf sh. (ManifoldRepr leaf) => Sing sh -> BVar r (RecordTree leaf (RegularTreeShapeLeaf sh)) -> BVar r (leaf @@ sh)
unwrapBVarLeafTree sh (BVar (RecordTree x) dx) = case manifoldReprDict @leaf sh of
    Dict -> BVar x (inlineNode unRecordTree RecordTreeVecBuilder dx)

bvarSubtreeFields ::
    forall r record leaf.
    (IsRegularRecord record, HasGradRepr leaf, ManifoldRepr leaf) =>
    BVar r (RecordTree leaf (RegularTreeShapeSubtree record)) ->
    BVar r (HListOf (TyCon1 (RecordTree leaf)) (FieldShapes record))
bvarSubtreeFields (BVar x dx) = BVar (toRecordTreeFields x) (projectNode toRecordTreeFields intoRec dx)
  where
    intoRec ::
        HListBuilder (TyCon1 (RecordTree (ReprGrad leaf))) (FieldShapes record) ->
        RecordTreeVecBuilder (ReprGrad leaf) (RegularTreeShapeSubtree record)
    intoRec = RecordTreeVecBuilder . sparseBuilder

bvarSubtreeFromFields ::
    forall r record leaf.
    (IsRegularRecord record, HasGradRepr leaf, ManifoldRepr leaf) =>
    BVar r (HListOf (TyCon1 (RecordTree leaf)) (FieldShapes record)) ->
    BVar r (RecordTree leaf (RegularTreeShapeSubtree record))
bvarSubtreeFromFields (BVar x dx) = BVar (fromRecordTreeFields x) (projectNode fromRecordTreeFields fromRec dx)
  where
    fromRec ::
        RecordTreeVecBuilder (ReprGrad leaf) (RegularTreeShapeSubtree record) ->
        HListBuilder (TyCon1 (RecordTree (ReprGrad leaf))) (FieldShapes record)
    fromRec = flattenDenseSparseBuilder (identityBuilder . toRecordTreeFields) . unRecordTreeVecBuilder

splitLeaf ::
    forall r leaf sh.
    (BVarRepr leaf, ManifoldRepr leaf) =>
    Sing sh ->
    BVar r (RecordTree leaf (RegularTreeShapeLeaf sh)) ->
    RecordTree (ReprBVar r leaf) (RegularTreeShapeLeaf sh)
splitLeaf sh = RecordTree . splitBVarField @leaf sh . unwrapBVarLeafTree sh

joinLeaf ::
    forall r leaf sh.
    (BVarRepr leaf, ManifoldRepr leaf) =>
    Sing sh ->
    RecordTree (ReprBVar r leaf) (RegularTreeShapeLeaf sh) ->
    BVar r (RecordTree leaf (RegularTreeShapeLeaf sh))
joinLeaf sh = wrapBVarLeafTree sh . joinBVarField @leaf sh . unRecordTree

splitRecord ::
    forall r record leaf.
    (BVarRepr leaf, ManifoldRepr leaf, IsRegularRecord record, HasGradRepr leaf) =>
    BVar r (RecordTree leaf (RegularTreeShapeSubtree record)) -> RecordTree (ReprBVar r leaf) (RegularTreeShapeSubtree record)
splitRecord = fromRecordTreeFields . splitBVarHList sing . bvarSubtreeFields

joinRecord ::
    forall r record leaf.
    (BVarRepr leaf, ManifoldRepr leaf, IsRegularRecord record, HasGradRepr leaf) =>
    RecordTree (ReprBVar r leaf) (RegularTreeShapeSubtree record) -> BVar r (RecordTree leaf (RegularTreeShapeSubtree record))
joinRecord = bvarSubtreeFromFields @r @record @leaf . joinBVarHList sing . toRecordTreeFields

{- | Differentiate a function written against a regular record of variables.

The input record is split recursively, while leaves are converted according to
the leaf representation's 'BVarRepr' instance.
-}
adRecordTree ::
    forall {k} (leaf :: k ~> Type) record y.
    ( BVarRepr leaf
    , ManifoldRepr leaf
    , HasGradRepr leaf
    , IsRegularRecord record
    , HasBuilder (Tang y)
    , HasBuilder (Grad y)
    , DualOf (DualOf (Tang y)) ~ Tang y
    ) =>
    (forall r. record (ReprBVar r leaf) -> BVar r y) ->
    DFun (RecordTree leaf ('RegularTreeShapeSubtree record)) y
adRecordTree f = BVar.ad (f . unRecordTree . splitRecord)

instance (BVarRepr leaf, ManifoldRepr leaf, HasGradRepr leaf) => BVarRepr (TyCon1 (RecordTree leaf)) where
    type ReprBVar r (TyCon1 (RecordTree leaf)) = TyCon1 (RecordTree (ReprBVar r leaf))
    splitBVarField (SRegularTreeShapeLeaf sh) = splitLeaf sh
    splitBVarField SRegularTreeShapeSubtree = splitRecord
    joinBVarField (SRegularTreeShapeLeaf sh) = joinLeaf sh
    joinBVarField SRegularTreeShapeSubtree = joinRecord

toRecordTreeFields ::
    forall {k} (record :: (k ~> Type) -> Type) (leaf :: k ~> Type).
    (IsRegularRecord record) => RecordTree leaf ('RegularTreeShapeSubtree record) -> HListOf (TyCon1 (RecordTree leaf)) (FieldShapes record)
toRecordTreeFields = wrapRecordTrees @leaf (sing @(FieldShapes record)) . toRegularRecordFields . unRecordTree

fromRecordTreeFields ::
    forall {k} (record :: (k ~> Type) -> Type) (leaf :: k ~> Type).
    (IsRegularRecord record) => HListOf (TyCon1 (RecordTree leaf)) (FieldShapes record) -> RecordTree leaf ('RegularTreeShapeSubtree record)
fromRecordTreeFields = RecordTree . fromRegularRecordFields . unwrapRecordTrees @leaf (sing @(FieldShapes record))

data ProxyLeaf :: k ~> Type
type instance Apply ProxyLeaf sh = Proxy sh

{- | Whether the outer constructor uses record syntax, independently of the
shapes or naming of any nested records.
-}
type family GenericFieldNaming (record :: (k ~> Type) -> Type) :: FieldNaming where
    GenericFieldNaming record = GenericFieldNamingFromRep (Rep (record ProxyLeaf))

type family GenericFieldNamingFromRep recordRep :: FieldNaming where
    GenericFieldNamingFromRep (M1 D _ (M1 C ('MetaCons _ _ 'True) _)) = 'Named
    GenericFieldNamingFromRep (M1 D _ (M1 C ('MetaCons _ _ 'False) _)) = 'Unnamed
    GenericFieldNamingFromRep (M1 D _ (_ :+: _)) =
        TypeError ('Text "Generic regular-record derivation does not support sum types")
    GenericFieldNamingFromRep (M1 D _ V1) =
        TypeError ('Text "Generic regular-record derivation does not support empty data types")
    GenericFieldNamingFromRep _ =
        TypeError ('Text "Generic regular-record derivation supports only single-constructor product types")

type family GenericRegularFieldShape field where
    GenericRegularFieldShape (Proxy sh) = 'RegularTreeShapeLeaf sh
    GenericRegularFieldShape (record ProxyLeaf) = 'RegularTreeShapeSubtree record
    GenericRegularFieldShape _ =
        TypeError ('Text "Generic regular-record fields must be leaves or nested regular-record records")

type family GenericRegularFieldShapeList fields (tail :: [RegularTreeShape k]) :: [RegularTreeShape k] where
    GenericRegularFieldShapeList (M1 S _ (Rec0 field)) tail = GenericRegularFieldShape field ': tail
    GenericRegularFieldShapeList (left :*: right) tail = GenericRegularFieldShapeList left (GenericRegularFieldShapeList right tail)
    GenericRegularFieldShapeList (M1 _ _ fields) tail = GenericRegularFieldShapeList fields tail
    GenericRegularFieldShapeList U1 _ = TypeError ('Text "Don't know what to do with empty records")
    GenericRegularFieldShapeList _ _ = TypeError ('Text "Generic regular-record derivation supports only product fields here")

type family GenericRegularShapeFromRep recordRep :: [RegularTreeShape k] where
    GenericRegularShapeFromRep (M1 D _ (M1 C _ fields)) = GenericRegularFieldShapeList fields '[]
    GenericRegularShapeFromRep (M1 D _ (_ :+: _)) =
        TypeError ('Text "Generic regular-record derivation does not support sum types")
    GenericRegularShapeFromRep (M1 D _ V1) =
        TypeError ('Text "Generic regular-record derivation does not support empty data types")
    GenericRegularShapeFromRep _ =
        TypeError ('Text "Generic regular-record derivation supports only single-constructor product types")

type family GenericFieldShapes (record :: (leafSh ~> Type) -> Type) :: [RegularTreeShape leafSh] where
    GenericFieldShapes record = GenericRegularShapeFromRep (Rep (record ProxyLeaf))

class GenericRegularRecord record proxyRep valueRep (leaf :: k ~> Type) where
    genericToRegularFieldsRep :: valueRep p -> HListOf (RegularTreeRep leaf) (FieldShapes record)
    genericFromRegularFieldsRep :: HListOf (RegularTreeRep leaf) (FieldShapes record) -> valueRep p

instance
    ( GenericRegularFieldsRep proxyFields valueFields leaf '[]
    , GenericRegularFieldShapeList proxyFields '[] ~ FieldShapes record
    ) =>
    GenericRegularRecord
        record
        (M1 D ('MetaData name moduleName packageName isNewtype) (M1 C constructorMeta proxyFields))
        (M1 D ('MetaData name moduleName packageName isNewtype) (M1 C constructorMeta' valueFields))
        leaf
    where
    genericToRegularFieldsRep (M1 (M1 fields)) = genericToRegularFieldsRepTupleFields @_ @proxyFields @valueFields @leaf @'[] fields HNilOf

    genericFromRegularFieldsRep record = M1 (M1 recordFields)
      where
        recordFields =
            case genericFromRegularFieldsRepTupleFields @_ @proxyFields @valueFields @leaf @'[] record of
                (fields, HNilOf) -> fields

class GenericRegularFieldsRep proxyFields valueFields (leaf :: k ~> Type) (tail2 :: [RegularTreeShape k]) where
    genericToRegularFieldsRepTupleFields ::
        forall p.
        valueFields p ->
        HListOf (RegularTreeRep leaf) tail2 ->
        HListOf (RegularTreeRep leaf) (GenericRegularFieldShapeList proxyFields tail2)
    genericFromRegularFieldsRepTupleFields ::
        forall p.
        HListOf (RegularTreeRep leaf) (GenericRegularFieldShapeList proxyFields tail2) ->
        (valueFields p, HListOf (RegularTreeRep leaf) tail2)

instance
    (valueField ~ RegularTreeRep leaf @@ GenericRegularFieldShape proxyField) =>
    GenericRegularFieldsRep
        (M1 S ('MetaSel name unpackedness strictness decidedStrictness) (Rec0 proxyField))
        (M1 S selectorMeta (Rec0 valueField))
        leaf
        tail
    where
    genericToRegularFieldsRepTupleFields (M1 (K1 field)) = HConsOf field

    genericFromRegularFieldsRepTupleFields (HConsOf field fields) = (M1 (K1 field), fields)

instance
    ( GenericRegularFieldsRep proxyLeft valueLeft leaf (GenericRegularFieldShapeList proxyRight tail2)
    , GenericRegularFieldsRep proxyRight valueRight leaf tail2
    ) =>
    GenericRegularFieldsRep
        (proxyLeft :*: proxyRight)
        (valueLeft :*: valueRight)
        leaf
        tail2
    where
    genericToRegularFieldsRepTupleFields ::
        forall p.
        (valueLeft :*: valueRight) p ->
        HListOf (RegularTreeRep leaf) tail2 ->
        HListOf (RegularTreeRep leaf) (GenericRegularFieldShapeList (proxyLeft :*: proxyRight) tail2)
    genericToRegularFieldsRepTupleFields (left :*: right) fieldsTail =
        genericToRegularFieldsRepTupleFields @_ @proxyLeft @valueLeft @leaf @(GenericRegularFieldShapeList proxyRight tail2)
            left
            (genericToRegularFieldsRepTupleFields @_ @proxyRight @valueRight @leaf @tail2 right fieldsTail)

    genericFromRegularFieldsRepTupleFields ::
        forall p.
        HListOf (RegularTreeRep leaf) (GenericRegularFieldShapeList (proxyLeft :*: proxyRight) tail2) ->
        ((valueLeft :*: valueRight) p, HListOf (RegularTreeRep leaf) tail2)
    genericFromRegularFieldsRepTupleFields fields =
        let (left, rest) = genericFromRegularFieldsRepTupleFields @_ @proxyLeft @valueLeft @leaf @(GenericRegularFieldShapeList proxyRight tail2) fields
            (right, rest') = genericFromRegularFieldsRepTupleFields @_ @proxyRight @valueRight @leaf @tail2 rest
         in (left :*: right, rest')

type GenericRegularFieldNameList :: forall {k1} {k2}. (k1 -> Type) -> [RegularTreeShape k2] -> Constraint
class GenericRegularFieldNameList proxyFields (tail2 :: [RegularTreeShape k]) where
    genericToRegularFieldNames :: HListOf (ConstSym1 Data.Text.Text) tail2 -> HListOf (ConstSym1 Data.Text.Text) (GenericRegularFieldShapeList proxyFields tail2)

instance (GenericRegularFieldNameList fields tail) => GenericRegularFieldNameList (M1 D metadata fields) tail where
    genericToRegularFieldNames = genericToRegularFieldNames @fields @tail

instance (GenericRegularFieldNameList fields tail) => GenericRegularFieldNameList (M1 C metadata fields) tail where
    genericToRegularFieldNames = genericToRegularFieldNames @fields @tail

instance (KnownSymbol name) => GenericRegularFieldNameList (M1 S ('MetaSel ('Just name) unpackedness strictness decidedStrictness) (Rec0 proxyField)) tail where
    genericToRegularFieldNames = HConsOf (Data.Text.pack $ symbolVal @name Proxy)

instance
    ( GenericRegularFieldNameList proxyLeft (GenericRegularFieldShapeList proxyRight tail2)
    , GenericRegularFieldNameList proxyRight tail2
    ) =>
    GenericRegularFieldNameList
        (proxyLeft :*: proxyRight)
        tail2
    where
    genericToRegularFieldNames ::
        HListOf (ConstSym1 Data.Text.Text) tail2 ->
        HListOf (ConstSym1 Data.Text.Text) (GenericRegularFieldShapeList (proxyLeft :*: proxyRight) tail2)
    genericToRegularFieldNames fieldsTail =
        genericToRegularFieldNames @proxyLeft @(GenericRegularFieldShapeList proxyRight tail2)
            (genericToRegularFieldNames @proxyRight @tail2 fieldsTail)

type GenericRegularFieldNames :: forall {k1} {k2}. FieldNaming -> k1 -> [RegularTreeShape k2] -> Constraint
class GenericRegularFieldNames (naming :: FieldNaming) proxyFields (fields :: [RegularTreeShape k]) where
    genericFieldNames :: RecordFieldNames naming fields

instance GenericRegularFieldNames 'Unnamed proxyFields fields where
    genericFieldNames = UnnamedFields

instance
    ( GenericRegularFieldNameList proxyFields ('[] :: [RegularTreeShape k2])
    , fields ~ GenericRegularFieldShapeList proxyFields '[]
    ) =>
    GenericRegularFieldNames 'Named proxyFields (fields :: [RegularTreeShape k2])
    where
    genericFieldNames = NamedFields (genericToRegularFieldNames @proxyFields HNilOf)
