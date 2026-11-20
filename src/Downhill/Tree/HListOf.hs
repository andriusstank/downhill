{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A recursively stored heterogeneous product and its vector builder.
module Downhill.Tree.HListOf (
    HListOf (..),
    fillHListOf,
    mapHListOf,
    zipHListOf,
    foldMapHListOf,
    foldZipHListOf,
    traverseHListOf,
    HListBuilder (..),
    splitBVarHList,
    joinBVarHList,
)
where

import Data.Constraint (Dict (Dict))
import Data.List.Singletons (SList (..))
import Data.Singletons (Apply, Sing, SingI (sing), TyCon1, withSingI, type (@@), type (~>))
import Downhill.BVar as BVar
import Downhill.Linear.AD (SparseFun2 (..), flipSparseFun2, realNode', sparseNode2, sparseTerm2)
import Downhill.Linear.Builder (HasBuilder (..), Sparse)
import Downhill.Linear.Expr (Expr (ExprSum), Term, VecPair (..))
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.Representation (
    AdditiveRepr,
    AllConstrained (allConstrained),
    BuilderRepr,
    HasGradRepr,
    ManifoldRepr,
    MultiplicativeActionOver,
    MultiplicativeActionRepr,
    PointwiseManifold (manifoldDict),
    PointwiseVSpace (vspaceDict),
    ReprDualOf,
    ReprGrad,
    ReprScalar,
    ReprTang,
    SubtractiveRepr,
    VSpaceRepr,
    hasGradReprDict,
 )
import Downhill.VSpace (VSpace (..))
import NumHask.Prelude

data family HListOf (f :: k ~> Type) (fields :: [k]) :: Type
data instance HListOf f '[] = HNilOf
data instance HListOf f (x ': xs) = HConsOf (f @@ x) (HListOf f xs)

fillHListOf :: forall {k} (f :: k ~> Type) fields. Sing fields -> (forall field. Sing field -> f @@ field) -> HListOf f fields
fillHListOf SNil _ = HNilOf
fillHListOf (SCons field fields) f = HConsOf (f field) (fillHListOf @f fields f)

mapHListOf ::
    forall {k} (f :: k ~> Type) (g :: k ~> Type) fields.
    (forall field. Sing field -> f @@ field -> g @@ field) ->
    Sing fields ->
    HListOf f fields ->
    HListOf g fields
mapHListOf f = go
  where
    go :: forall x. Sing x -> HListOf f x -> HListOf g x
    go SNil HNilOf = HNilOf
    go (SCons field fields) (HConsOf x xs) = HConsOf (f field x) (go fields xs)

foldMapHListOf ::
    forall f m fields.
    (Monoid m) =>
    (forall field. Sing field -> f @@ field -> m) -> Sing fields -> HListOf f fields -> m
foldMapHListOf f = go
  where
    go :: forall x. Sing x -> HListOf f x -> m
    go SNil HNilOf = mempty
    go (SCons shHead shTail) (HConsOf x xs) = f shHead x <> go shTail xs

foldZipHListOf ::
    forall f g m fields.
    (Monoid m) =>
    (forall field. Sing field -> f @@ field -> g @@ field -> m) -> Sing fields -> HListOf f fields -> HListOf g fields -> m
foldZipHListOf f = go
  where
    go :: forall x. Sing x -> HListOf f x -> HListOf g x -> m
    go SNil HNilOf HNilOf = mempty
    go (SCons shHead shTail) (HConsOf x xs) (HConsOf y ys) = f shHead x y <> go shTail xs ys

-- | Traverse every field in shape order, possibly changing its representation.
traverseHListOf ::
    forall {k} (f :: k ~> Type) (g :: k ~> Type) fields m.
    (Applicative m) =>
    (forall field. Sing field -> f @@ field -> m (g @@ field)) ->
    Sing fields ->
    HListOf f fields ->
    m (HListOf g fields)
traverseHListOf f = go
  where
    go :: forall x. Sing x -> HListOf f x -> m (HListOf g x)
    go SNil HNilOf = pure HNilOf
    go (SCons field fields) (HConsOf x xs) = HConsOf <$> f field x <*> go fields xs

zipHListOf ::
    forall {k} (f :: k ~> Type) (g :: k ~> Type) (h :: k ~> Type) fields.
    (forall field. Sing field -> f @@ field -> g @@ field -> h @@ field) ->
    Sing fields ->
    HListOf f fields ->
    HListOf g fields ->
    HListOf h fields
zipHListOf f = go
  where
    go :: forall x. Sing x -> HListOf f x -> HListOf g x -> HListOf h x
    go SNil HNilOf HNilOf = HNilOf
    go (SCons field fields) (HConsOf x xs) (HConsOf y ys) =
        HConsOf (f field x y) (go fields xs ys)

instance (SingI fields, AllConstrained Eq f) => Eq (HListOf f fields) where
    xs == ys = getAll (foldZipHListOf @f @f eqField (sing @fields) xs ys)
      where
        eqField :: forall field. Sing field -> f @@ field -> f @@ field -> All
        eqField field x y = case allConstrained @Eq @f field of
            Dict -> All (x == y)

instance (SingI fields, AllConstrained Show f) => Show (HListOf f fields) where
    showsPrec p fields =
        showParen (p > 10) $ showString "HListOf " . showFields (fieldShows (sing @fields) fields)
      where
        fieldShows :: Sing fields' -> HListOf f fields' -> [ShowS]
        fieldShows SNil HNilOf = []
        fieldShows (SCons field rest) (HConsOf x xs) =
            case allConstrained @Show @f field of
                Dict -> shows x : fieldShows rest xs

        showFields :: [ShowS] -> ShowS
        showFields xs = showChar '[' . go xs . showChar ']'

        go :: [ShowS] -> ShowS
        go [] = id
        go [x] = x
        go (x : xs) = x . showChar ',' . go xs

instance (SingI fields, AdditiveRepr f) => Additive (HListOf f fields) where
    zero = fillHListOf @f (sing @fields) zeroField
      where
        zeroField :: forall field. Sing field -> f @@ field
        zeroField field = case allConstrained @Additive @f field of
            Dict -> zero

    xs + ys = zipHListOf @f @f @f addField (sing @fields) xs ys
      where
        addField :: forall field. Sing field -> f @@ field -> f @@ field -> f @@ field
        addField field x y = case allConstrained @Additive @f field of
            Dict -> x + y

instance (SingI fields, SubtractiveRepr f) => Subtractive (HListOf f fields) where
    negate = mapHListOf @f @f negateField (sing @fields)
      where
        negateField :: forall field. Sing field -> f @@ field -> f @@ field
        negateField field x = case allConstrained @Subtractive @f field of
            Dict -> negate x

    xs - ys = zipHListOf @f @f @f subtractField (sing @fields) xs ys
      where
        subtractField :: forall field. Sing field -> f @@ field -> f @@ field -> f @@ field
        subtractField field x y = case allConstrained @Subtractive @f field of
            Dict -> x - y

instance (SingI fields, AllConstrained Multiplicative f) => Multiplicative (HListOf f fields) where
    one = fillHListOf @f (sing @fields) oneField
      where
        oneField :: forall field. Sing field -> f @@ field
        oneField field = case allConstrained @Multiplicative @f field of
            Dict -> one

    xs * ys = zipHListOf @f @f @f multiplyField (sing @fields) xs ys
      where
        multiplyField :: forall field. Sing field -> f @@ field -> f @@ field -> f @@ field
        multiplyField field x y = case allConstrained @Multiplicative @f field of
            Dict -> x * y

instance (SingI fields, MultiplicativeActionRepr f) => MultiplicativeAction (HListOf f fields) where
    type Scalar (HListOf f fields) = ReprScalar f
    xs |* a = mapHListOf @f @f scaleField (sing @fields) xs
      where
        scaleField :: forall field. Sing field -> f @@ field -> f @@ field
        scaleField field x = case allConstrained @(MultiplicativeActionOver (ReprScalar f)) @f field of
            Dict -> x |* a

type instance DualOf (HListOf f fields) = HListOf (ReprDualOf f) fields

instance (SingI fields, VSpaceRepr f) => VSpace (HListOf f fields) where
    dx |*| x = getSum (foldZipHListOf @(ReprDualOf f) @f multiplyField (sing @fields) dx x)
      where
        multiplyField :: forall field. Sing field -> ReprDualOf f @@ field -> f @@ field -> Sum (ReprScalar f)
        multiplyField field dx' x' = case vspaceDict @f field of
            Dict -> Sum (dx' |*| x')

type instance Tang (HListOf f fields) = HListOf (ReprTang f) fields

instance (SingI fields, ManifoldRepr f) => Manifold (HListOf f fields) where
    x .+~^ dx = zipHListOf @f @(ReprTang f) @f addTangentField (sing @fields) x dx
      where
        addTangentField :: forall field. Sing field -> f @@ field -> ReprTang f @@ field -> f @@ field
        addTangentField field x' dx' = case manifoldDict @f field of
            Dict -> x' .+~^ dx'

    x .-~^ dx = zipHListOf @f @(ReprTang f) @f subtractTangentField (sing @fields) x dx
      where
        subtractTangentField :: forall field. Sing field -> f @@ field -> ReprTang f @@ field -> f @@ field
        subtractTangentField field x' dx' = case manifoldDict @f field of
            Dict -> x' .-~^ dx'

data family HListBuilder (f :: k ~> Type) (fields :: [k])

data instance HListBuilder f '[] = HNilBuilder
data instance HListBuilder f (shHead ': shTail) where
    HEmptyBuilder :: HListBuilder f (shHead ': shTail)
    HDenseBuilder :: Sum (HListOf f (shHead ': shTail)) -> HListBuilder f (shHead ': shTail)
    HConsSparseBuilder :: VecBuilder (f @@ shHead) -> HListBuilder f shTail -> HListBuilder f (shHead ': shTail)

mappendHCons ::
    forall f shHead shTail.
    (HasBuilder (f @@ shHead), SingI shTail, BuilderRepr f) =>
    HListBuilder f (shHead ': shTail) -> HListBuilder f (shHead ': shTail) -> HListBuilder f (shHead ': shTail)
mappendHCons HEmptyBuilder x = x
mappendHCons x HEmptyBuilder = x
mappendHCons x y = HConsSparseBuilder (xHead <> yHead) (xTail <> yTail)
  where
    unconsBuilder :: HListBuilder f (shHead ': shTail) -> (VecBuilder (f @@ shHead), HListBuilder f shTail)
    unconsBuilder HEmptyBuilder = (mempty, mempty)
    unconsBuilder (HDenseBuilder (Sum (HConsOf z zs))) = (identityBuilder z, identityBuilder zs)
    unconsBuilder (HConsSparseBuilder z zs) = (z, zs)

    (xHead, xTail) = unconsBuilder x
    (yHead, yTail) = unconsBuilder y

instance (SingI fields, BuilderRepr f) => Semigroup (HListBuilder f fields) where
    x <> y = case sing @fields of
        SNil -> HNilBuilder
        SCons field fields ->
            case allConstrained @HasBuilder @f field of
                Dict -> withSingI field $ withSingI fields $ mappendHCons x y

instance (SingI fields, BuilderRepr f) => Monoid (HListBuilder f fields) where
    mempty = case sing @fields of
        SNil -> HNilBuilder
        SCons _ _ -> HEmptyBuilder

sumBuilderS :: forall f sh. (BuilderRepr f) => Sing sh -> VecBuilder (f @@ sh) -> f @@ sh
sumBuilderS sh x = case allConstrained @HasBuilder @f sh of
    Dict -> sumBuilder x

sumHListBuilder :: forall f fields. (BuilderRepr f) => Sing fields -> HListBuilder f fields -> HListOf f fields
sumHListBuilder sh = case sh of
    SNil -> \HNilBuilder -> HNilOf
    SCons shHead shTail -> \case
        HEmptyBuilder -> withSingI sh zero
        HDenseBuilder (Sum x) -> x
        HConsSparseBuilder x xs -> HConsOf (sumBuilderS @f shHead x) (sumHListBuilder shTail xs)

instance (SingI fields, BuilderRepr f) => HasBuilder (HListOf f fields) where
    type VecBuilder (HListOf f fields) = HListBuilder f fields
    sumBuilder = sumHListBuilder (sing @fields)
    identityBuilder = case sing @fields of
        SNil -> const HNilBuilder
        SCons _ _ -> HDenseBuilder . Sum

unconsBVarHListHelper ::
    forall r f shHead shTail.
    ( SingI shTail
    , SingI shHead
    , BuilderRepr (ReprTang f)
    , BuilderRepr (ReprGrad f)
    , HasBuilder (ReprTang f @@ shHead)
    , HasBuilder (ReprGrad f @@ shHead)
    , ReprTang f @@ shHead ~ Tang (f @@ shHead)
    , ReprGrad f @@ shHead ~ DualOf (ReprTang f @@ shHead)
    ) =>
    BVar r (HListOf f (shHead : shTail)) -> (BVar r (f @@ shHead), BVar r (HListOf f shTail))
unconsBVarHListHelper (BVar (HConsOf x xs) dx) = (bx2, bxs2)
  where
    bx2 :: BVar r (f @@ shHead)
    bx2 = BVar x (sparseNode2 headFun dx)
    bxs2 :: BVar r (HListOf f shTail)
    bxs2 = BVar xs (sparseNode2 tailFun dx)

splitBVarHList ::
    forall f r sh.
    ( BVarRepr f
    , ManifoldRepr f
    , BuilderRepr (ReprTang f)
    , HasGradRepr f
    ) =>
    Sing sh -> BVar r (HListOf f sh) -> HListOf (ReprBVar r f) sh
splitBVarHList SNil (BVar HNilOf _) = HNilOf
splitBVarHList (SCons shHead shTail) bx = case hasGradReprDict @f shHead of
    Dict -> case withSingI shHead $ withSingI shTail unconsBVarHListHelper bx of
        (bHead, bTail) -> HConsOf (splitBVarField @f @r shHead bHead) (splitBVarHList shTail bTail)

headFun ::
    forall f shHead shTail.
    ( SingI shHead
    , SingI shTail
    , BuilderRepr f
    , BuilderRepr (ReprDualOf f)
    , HasBuilder (f @@ shHead)
    , HasBuilder (ReprDualOf f @@ shHead)
    , ReprDualOf f @@ shHead ~ DualOf (f @@ shHead)
    ) =>
    SparseFun2 (HListOf f (shHead : shTail)) (f @@ shHead)
headFun = SparseFun2 fwd back
  where
    fwd :: HListBuilder f (shHead : shTail) -> VecBuilder (f @@ shHead)
    fwd HEmptyBuilder = mempty
    fwd (HDenseBuilder (Sum (HConsOf x _))) = identityBuilder x
    fwd (HConsSparseBuilder x _) = x
    back :: VecBuilder (DualOf (Apply f shHead)) -> HListBuilder (ReprDualOf f) (shHead : shTail)
    back x = HConsSparseBuilder x mempty

{-# ANN tailFun ("HLint: ignore Eta reduce" :: String) #-}
tailFun ::
    forall f shHead shTail.
    ( SingI shHead
    , SingI shTail
    , BuilderRepr f
    , BuilderRepr (ReprDualOf f)
    , HasBuilder (f @@ shHead)
    , HasBuilder (ReprDualOf f @@ shHead)
    ) =>
    SparseFun2 (HListOf f (shHead : shTail)) (HListOf f shTail)
tailFun = SparseFun2 fwd back
  where
    fwd :: HListBuilder f (shHead : shTail) -> HListBuilder f shTail
    fwd HEmptyBuilder = mempty
    fwd (HDenseBuilder (Sum (HConsOf _ xs))) = identityBuilder xs
    fwd (HConsSparseBuilder _ xs) = xs
    back :: HListBuilder (ReprDualOf f) shTail -> HListBuilder (ReprDualOf f) (shHead : shTail)
    back x = HConsSparseBuilder mempty x

consBVarHListHelper ::
    forall r f shHead shTail.
    ( SingI shTail
    , SingI shHead
    , BuilderRepr (ReprTang f)
    , BuilderRepr (ReprGrad f)
    , HasBuilder (ReprTang f @@ shHead)
    , HasBuilder (ReprGrad f @@ shHead)
    , ReprTang f @@ shHead ~ Tang (f @@ shHead)
    , ReprGrad f @@ shHead ~ DualOf (ReprTang f @@ shHead)
    , ReprDualOf (ReprDualOf (ReprTang f)) ~ ReprTang f
    , DualOf (DualOf (Tang (Apply f shHead))) ~ Tang (Apply f shHead)
    ) =>
    BVar r (f @@ shHead) -> BVar r (HListOf f shTail) -> BVar r (HListOf f (shHead : shTail))
consBVarHListHelper (BVar x dx) (BVar xs dxs) = BVar (HConsOf x xs) (realNode' (ExprSum [t1, t2]))
  where
    t1, t2 :: Term ('VecPair r (DualOf r)) ('VecPair (HListOf (ReprTang f) (shHead : shTail)) (Sparse (HListOf (ReprDualOf (ReprTang f)) (shHead : shTail))))
    t1 = sparseTerm2 (flipSparseFun2 headFun) dx
    t2 = sparseTerm2 (flipSparseFun2 tailFun) dxs

joinBVarHList ::
    forall f r sh.
    ( BVarRepr f
    , ManifoldRepr f
    , BuilderRepr (ReprTang f)
    , HasGradRepr f
    ) =>
    Sing sh -> HListOf (ReprBVar r f) sh -> BVar r (HListOf f sh)
joinBVarHList SNil HNilOf = BVar.constant HNilOf
joinBVarHList (SCons shHead shTail) (HConsOf x xs) = case hasGradReprDict @f shHead of
    Dict -> withSingI shHead $ withSingI shTail $ consBVarHListHelper (joinBVarField @f @r shHead x) (joinBVarHList shTail xs)

instance
    ( BVarRepr f
    , ManifoldRepr f
    , BuilderRepr (ReprTang f)
    , HasGradRepr f
    ) =>
    BVarRepr (TyCon1 (HListOf f))
    where
    type ReprBVar r (TyCon1 (HListOf f)) = TyCon1 (HListOf (ReprBVar r f))
    splitBVarField = splitBVarHList
    joinBVarField = joinBVarHList
