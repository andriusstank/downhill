{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Tree (
  TreeShape (..),
  TreeOf,
  Tree (..),
  adTree,
  adTreeRecord,
  fillTreeEx,
  mapTreeEx,
  zipTreeEx,
  foldTreeEx,
  foldMapTreeEx,
  traverseTreeEx,
)
where

import Data.Constraint (Dict (..))
import Control.Monad.ST.Strict (ST, runST)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.STRef.Strict (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Singletons (Apply, Sing, SingI (sing), TyCon1, withSingI, type (@@), type (~>))
import Downhill.BVar (BVar (..), DFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.AD (AD (..), inlineNode, projectNode, realNode)
import Downhill.Linear.Builder (HasBuilder (VecBuilder, identityBuilder, sumBuilder))
import Downhill.Linear.Expr (BiFun (BiFun), Expr (ExprSum), Term, VecPair (..))
import Downhill.Linear.Fun (AsLinearMap (..), DualOf, LinearMap (..), Metric)
import Downhill.Manifold (Grad, Manifold (..), Tang)
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.Record (FieldGetter (..), IsRecord (..), SShR (SShR), ShR (ShR), foldZipRecordEx)
import Downhill.VSpace (InnerSpace (..), VSpace ((|*|)))
import NumHask.Prelude
import Downhill.Tree.Representation (AllConstrained (allConstrained), PointwiseManifold (manifoldDict), PointwiseVSpace (vspaceDict), ReprDualOf, ReprTang, ReprGrad, ReprScalar)
import Downhill.Tree.Representation qualified as Representation

-- | Shape of a tree.
--
-- A tree is either a leaf with some leaf shape, or a record node whose fields
-- are themselves trees. This keeps the tree structure independent from the
-- representation stored at the leaves.
data TreeShape leafShape = TreeShapeLeaf leafShape | TreeShapeRecord ((TreeShape leafShape ~> Type) -> Type)

data STreeShape (field :: TreeShape leafShape) where
  STreeLeaf :: Sing sh -> STreeShape ('TreeShapeLeaf sh)
  STreeRecord :: Sing ('ShR subtree) -> STreeShape ('TreeShapeRecord subtree)

type instance Sing = STreeShape

instance (SingI sh) => SingI ('TreeShapeLeaf sh) where
  sing = STreeLeaf sing

instance (IsRecord subtree) => SingI ('TreeShapeRecord subtree) where
  sing = STreeRecord sing

-- | The usual representation of a tree.
--
-- @TreeOf leaf@ takes a representation for leaves and applies it to every leaf
-- shape in the tree. Record nodes keep their original record structure, with
-- each field recursively represented as another @TreeOf leaf@.
--
-- For example, a leaf shape @sh@ contains @leaf @@ sh@, while a record-shaped
-- subtree @MyRecord@ contains @MyRecord (TreeOf leaf)@.
--
-- This is the dense, user-facing tree representation. Some internal machinery
-- uses other representations for builders and automatic differentiation.
data TreeOf (leaf :: leafShape ~> Type) :: TreeShape leafShape ~> Type
type instance Apply (TreeOf leaf) ('TreeShapeLeaf sh) = leaf @@ sh
type instance Apply (TreeOf leaf) ('TreeShapeRecord subtree) = subtree (TreeOf leaf)

-- | A tree value with a statically known shape.
--
-- 'TreeOf' describes how a tree is represented at each shape. 'Tree' gives
-- that type-family application a concrete type constructor on which the
-- library can define instances.
newtype Tree leaf sh = Tree {unTree :: TreeOf leaf @@ sh}

fillTreeEx :: forall a sh. (forall sh'. Sing sh' -> a @@ sh') -> Sing sh -> TreeOf a @@ sh
fillTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf a @@ sh'
  go (STreeLeaf sh) = f sh
  go (STreeRecord SShR) = fillRecordEx go


mapTreeEx :: forall a b sh. (forall sh'. Sing sh' -> a @@ sh' -> b @@ sh') -> Sing sh -> TreeOf a @@ sh -> TreeOf b @@ sh
mapTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf a @@ sh' -> TreeOf b @@ sh'
  go (STreeLeaf sh) = f sh
  go (STreeRecord SShR) = mapRecordEx go

zipTreeEx ::
  forall a b c sh.
  (forall sh'. Sing sh' -> a @@ sh' -> b @@ sh' -> c @@ sh') ->
  Sing sh ->
  TreeOf a @@ sh ->
  TreeOf b @@ sh ->
  TreeOf c @@ sh
zipTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf a @@ sh' -> TreeOf b @@ sh' -> TreeOf c @@ sh'
  go (STreeLeaf sh) = f sh
  go (STreeRecord SShR) = zipRecordEx go

foldTreeEx :: forall leaf b sh. (forall sh'. Sing sh' -> leaf @@ sh' -> b -> b) -> Sing sh -> b -> TreeOf leaf @@ sh -> b
foldTreeEx f sh' b' x' = go sh' x' b'
 where
  go :: forall sh'. Sing sh' -> TreeOf leaf @@ sh' -> b -> b
  go (STreeLeaf sh) x b = f sh x b
  go (STreeRecord SShR) x b = foldRecordEx go b x

foldMapTreeEx :: forall leaf a sh. (Monoid a) => (forall sh'. Sing sh' -> leaf @@ sh' -> a) -> Sing sh -> TreeOf leaf @@ sh -> a
foldMapTreeEx f sh = foldTreeEx @leaf @a @sh f' sh mempty
 where
  f' :: Sing sh' -> leaf @@ sh' -> a -> a
  f' sh' x a = f sh' x <> a

traverseTreeEx :: forall a b f sh. (Applicative f) => (forall sh'. Sing sh' -> a @@ sh' -> f (b @@ sh')) -> Sing sh -> TreeOf a @@ sh -> f (TreeOf b @@ sh)
traverseTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf a @@ sh' -> f (TreeOf b @@ sh')
  go (STreeLeaf sh) = f sh
  go (STreeRecord SShR) = mapRecordExA go

foldZipTreeEx ::
  forall a b m sh.
  (Monoid m) =>
  (forall sh'. Sing sh' -> a @@ sh' -> b @@ sh' -> m) ->
  Sing sh ->
  TreeOf a @@ sh ->
  TreeOf b @@ sh ->
  m
foldZipTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf a @@ sh' -> TreeOf b @@ sh' -> m
  go (STreeLeaf sh) x y = f sh x y
  go (STreeRecord SShR) x y = foldZipRecordEx go x y

fillTreeConstrained ::
  forall c leaf sh.
  (Representation.AllConstrained c leaf, SingI sh) =>
  (forall a. c a => a) ->
  Tree leaf sh
fillTreeConstrained x = Tree (fillTreeEx @leaf @sh go sing)
 where
  go :: forall sh'. Sing sh' -> leaf @@ sh'
  go sh = case Representation.allConstrained @c @leaf sh of
    Dict -> x

mapTreeConstrained ::
  forall c leaf sh.
  (Representation.AllConstrained c leaf, SingI sh) =>
  (forall a. c a => a -> a) ->
  Tree leaf sh ->
  Tree leaf sh
mapTreeConstrained f (Tree x) = Tree (mapTreeEx @leaf @leaf @sh go sing x)
 where
  go :: forall sh'. Sing sh' -> leaf @@ sh' -> leaf @@ sh'
  go sh x' = case Representation.allConstrained @c @leaf sh of
    Dict -> f x'

zipTreeConstrained ::
  forall c leaf sh.
  (Representation.AllConstrained c leaf, SingI sh) =>
  (forall a. c a => a -> a -> a) ->
  Tree leaf sh ->
  Tree leaf sh ->
  Tree leaf sh
zipTreeConstrained f (Tree x) (Tree y) = Tree (zipTreeEx @leaf @leaf @leaf @sh go sing x y)
 where
  go :: forall sh'. Sing sh' -> leaf @@ sh' -> leaf @@ sh' -> leaf @@ sh'
  go sh x' y' = case Representation.allConstrained @c @leaf sh of
    Dict -> f x' y'

type instance DualOf (Tree leaf sh) = Tree (ReprDualOf leaf) sh

instance (Representation.AdditiveRepr leaf, SingI sh) => Additive (Tree leaf sh) where
  zero = fillTreeConstrained @Additive zero
  (+) = zipTreeConstrained @Additive (+)

instance (Representation.SubtractiveRepr leaf, SingI sh) => Subtractive (Tree leaf sh) where
  negate = mapTreeConstrained @Subtractive negate
  (-) = zipTreeConstrained @Subtractive (-)

instance (Representation.MultiplicativeActionRepr leaf, SingI sh) => MultiplicativeAction (Tree leaf sh) where
  type Scalar (Tree leaf sh) = ReprScalar leaf
  v |* a = mapTreeConstrained @(Representation.MultiplicativeActionOver (ReprScalar leaf)) (|* a) v

instance (Representation.VSpaceRepr leaf, SingI sh) => VSpace (Tree leaf sh) where
  Tree dx |*| Tree x = getSum (foldZipTreeEx @(ReprDualOf leaf) @leaf @(Sum (ReprScalar leaf)) @sh go sing dx x)
   where
    go :: forall sh'. Sing sh' -> ReprDualOf leaf @@ sh' -> leaf @@ sh' -> Sum (ReprScalar leaf)
    go sh dx' x' = case Representation.vspaceDict @leaf sh of
      Dict -> Sum (dx' |*| x')

instance (Representation.InnerSpaceRepr leaf, SingI sh) => InnerSpace (Tree leaf sh) where
  riesz (Tree x) = Tree (mapTreeEx @leaf @(ReprDualOf leaf) @sh go sing x)
   where
    go :: forall sh'. Sing sh' -> leaf @@ sh' -> ReprDualOf leaf @@ sh'
    go sh x' = case Representation.innerSpaceReprDict @leaf sh of
      Dict -> riesz x'

  coriesz (Tree dx) = Tree (mapTreeEx @(ReprDualOf leaf) @leaf @sh go sing dx)
   where
    go :: forall sh'. Sing sh' -> ReprDualOf leaf @@ sh' -> leaf @@ sh'
    go sh dx' = case Representation.innerSpaceReprDict @leaf sh of
      Dict -> coriesz dx'

  Tree x <.> Tree y = getSum (foldZipTreeEx @leaf @leaf @(Sum (ReprScalar leaf)) @sh go sing x y)
   where
    go :: forall sh'. Sing sh' -> leaf @@ sh' -> leaf @@ sh' -> Sum (ReprScalar leaf)
    go sh x' y' = case Representation.innerSpaceReprDict @leaf sh of
      Dict -> Sum (x' <.> y')

type instance Tang (Tree leaf sh) = Tree (ReprTang leaf) sh

instance (Representation.ManifoldRepr leaf, SingI sh) => Manifold (Tree leaf sh) where
  Tree p .+~^ Tree v = Tree (zipTreeEx @leaf @(ReprTang leaf) @leaf @sh go sing p v)
   where
    go :: forall sh'. Sing sh' -> leaf @@ sh' -> ReprTang leaf @@ sh' -> leaf @@ sh'
    go sh = case Representation.manifoldDict @leaf sh of
      Dict -> (.+~^)

  Tree p .-~^ Tree v = Tree (zipTreeEx @leaf @(ReprTang leaf) @leaf @sh go sing p v)
   where
    go :: forall sh'. Sing sh' -> leaf @@ sh' -> ReprTang leaf @@ sh' -> leaf @@ sh'
    go sh = case Representation.manifoldDict @leaf sh of
      Dict -> (.-~^)

instance
  ( Representation.PointwiseMetric metric cov
  , Representation.VSpaceRepr cov
  , vec ~ ReprDualOf cov
  , cov ~ ReprDualOf vec
  , SingI sh
  ) =>
  AsLinearMap (Tree metric sh) (Tree cov sh) (Tree vec sh)
 where
  toLinearMap (Tree metrics) = LinearMap fwd back
   where
    fwd (Tree x) = Tree (zipTreeEx @metric @cov @vec @sh fwdLeaf sing metrics x)
    back (Tree x) = Tree (zipTreeEx @metric @cov @vec @sh backLeaf sing metrics x)

    fwdLeaf :: forall sh'. Sing sh' -> metric @@ sh' -> cov @@ sh' -> vec @@ sh'
    fwdLeaf sh m x = case Representation.metricDict @metric @cov sh of
      Dict -> case Representation.vspaceDict @cov sh of
        Dict -> lmapFwd (toLinearMap m) x

    backLeaf :: forall sh'. Sing sh' -> metric @@ sh' -> cov @@ sh' -> vec @@ sh'
    backLeaf sh m x = case Representation.metricDict @metric @cov sh of
      Dict -> case Representation.vspaceDict @cov sh of
        Dict -> lmapBack (toLinearMap @(metric @@ sh') @(cov @@ sh') m) x

instance
  (Representation.PointwiseMetric metric cov, Representation.VSpaceRepr cov, SingI sh) =>
  Metric (Tree metric sh) (Tree cov sh)

type instance ReprScalar (TyCon1 (Tree leaf)) = ReprScalar leaf
type instance ReprDualOf (TyCon1 (Tree leaf)) = TyCon1 (Tree (ReprDualOf leaf))
type instance ReprTang (TyCon1 (Tree leaf)) = TyCon1 (Tree (ReprTang leaf))

instance (forall sh. SingI sh => c (Tree leaf sh)) => Representation.AllConstrained c (TyCon1 (Tree leaf)) where
  allConstrained sh = withSingI sh Dict

instance (Representation.VSpaceRepr leaf) => Representation.PointwiseVSpace (TyCon1 (Tree leaf)) where
  vspaceDict (STreeLeaf sh) = withSingI sh Dict
  vspaceDict (STreeRecord SShR) = Dict

instance (Representation.ManifoldRepr leaf) => Representation.PointwiseManifold (TyCon1 (Tree leaf)) where
  manifoldDict (STreeLeaf sh) = withSingI sh Dict
  manifoldDict (STreeRecord SShR) = Dict

data TreeFieldBuilder record leaf where
  TreeFieldBuilder ::
    FieldGetter record sh ->
    TreeVecBuilder leaf sh ->
    TreeFieldBuilder record leaf

data TreeRecordBuilder leaf record = TreeRecordBuilder
  (Maybe (Sum (Tree leaf ('TreeShapeRecord record))))
  (DList (TreeFieldBuilder record leaf))

instance
  (IsRecord record, Representation.BuilderRepr leaf) =>
  Semigroup (TreeRecordBuilder leaf record)
 where
  TreeRecordBuilder x xs <> TreeRecordBuilder y ys =
    TreeRecordBuilder (x <> y) (xs <> ys)

instance
  (IsRecord record, Representation.BuilderRepr leaf) =>
  Monoid (TreeRecordBuilder leaf record)
 where
  mempty = TreeRecordBuilder mempty mempty

treeFieldBuilder ::
  FieldGetter record sh ->
  TreeVecBuilder leaf sh ->
  TreeRecordBuilder leaf record
treeFieldBuilder getter builder =
  TreeRecordBuilder Nothing (DList.singleton (TreeFieldBuilder getter builder))

data TreeBuilderAccum s (leaf :: k ~> Type) :: TreeShape k ~> Type
type instance Apply (TreeBuilderAccum s leaf) sh = STRef s (TreeVecBuilder leaf sh)

sumTreeRecordBuilder ::
  forall leaf record.
  (IsRecord record, Representation.BuilderRepr leaf) =>
  TreeRecordBuilder leaf record ->
  Tree leaf ('TreeShapeRecord record)
sumTreeRecordBuilder (TreeRecordBuilder densePart sparseParts) =
  case toList sparseParts of
    [] -> denseValue
    _ -> runST sumSparse
 where
  denseValue :: Tree leaf ('TreeShapeRecord record)
  denseValue = case densePart of
    Nothing -> zero
    Just (Sum tree) -> tree

  sumSparse :: forall s. ST s (Tree leaf ('TreeShapeRecord record))
  sumSparse = do
    accumulators <- mapRecordExA initField (unTree denseValue)
    traverse_ (addField accumulators) sparseParts
    Tree <$> mapRecordExA sumField accumulators

  initField ::
    forall s sh.
    Sing sh ->
    TreeOf leaf @@ sh ->
    ST s (TreeBuilderAccum s leaf @@ sh)
  initField sh value =
    withSingI sh (newSTRef (identityBuilder @(Tree leaf sh) (Tree value)))

  addField ::
    forall s.
    record (TreeBuilderAccum s leaf) ->
    TreeFieldBuilder record leaf ->
    ST s ()
  addField accumulators (TreeFieldBuilder getter builder) =
    withSingI (recordFieldSh getter) $
      modifySTRef (recordField getter accumulators) (<> builder)

  sumField ::
    forall s sh.
    Sing sh ->
    TreeBuilderAccum s leaf @@ sh ->
    ST s (TreeOf leaf @@ sh)
  sumField sh accumulator =
    withSingI sh (unTree . sumBuilder @(Tree leaf sh) <$> readSTRef accumulator)

type family TreeVecBuilderRep (leaf :: k ~> Type) (sh :: TreeShape k) :: Type where
  TreeVecBuilderRep leaf ('TreeShapeLeaf leafSh) = VecBuilder (leaf @@ leafSh)
  TreeVecBuilderRep leaf ('TreeShapeRecord record) =
    TreeRecordBuilder leaf record

newtype TreeVecBuilder (leaf :: k ~> Type) (sh :: TreeShape k) =
  TreeVecBuilder (TreeVecBuilderRep leaf sh)

instance (SingI sh, Representation.BuilderRepr leaf) => Semigroup (TreeVecBuilder leaf sh) where
  TreeVecBuilder x <> TreeVecBuilder y = TreeVecBuilder $ case sing @sh of
    STreeLeaf leafSh -> case Representation.allConstrained @HasBuilder @leaf leafSh of
      Dict -> x <> y
    STreeRecord SShR -> x <> y

instance (SingI sh, Representation.BuilderRepr leaf) => Monoid (TreeVecBuilder leaf sh) where
  mempty = TreeVecBuilder $ case sing @sh of
    STreeLeaf leafSh -> case Representation.allConstrained @HasBuilder @leaf leafSh of
      Dict -> mempty
    STreeRecord SShR -> mempty

instance (SingI sh, Representation.BuilderRepr leaf) => HasBuilder (Tree leaf sh) where
  type VecBuilder (Tree leaf sh) = TreeVecBuilder leaf sh

  sumBuilder (TreeVecBuilder builder) = case sing @sh of
    STreeLeaf leafSh -> case Representation.allConstrained @HasBuilder @leaf leafSh of
      Dict -> Tree (sumBuilder builder)
    STreeRecord SShR -> sumTreeRecordBuilder builder

  identityBuilder tree@(Tree x) = TreeVecBuilder $ case sing @sh of
    STreeLeaf leafSh -> case Representation.allConstrained @HasBuilder @leaf leafSh of
      Dict -> identityBuilder x
    STreeRecord SShR -> TreeRecordBuilder (Just (Sum tree)) mempty

data TreeBVarFields (r :: Type) (leaf :: k ~> Type) :: TreeShape k ~> Type
type instance Apply (TreeBVarFields r leaf) sh = BVar r (Tree leaf sh)

wrapBVarLeafTree ::
  forall r leaf leafSh.
  (Representation.HasGradRepr leaf) =>
  Sing leafSh ->
  BVar r (leaf @@ leafSh) ->
  BVar r (Tree leaf ('TreeShapeLeaf leafSh))
wrapBVarLeafTree leafSh (BVar x dx) = case Representation.hasGradReprDict @leaf leafSh of
  Dict -> BVar (Tree x) (inlineNode Tree unwrapBuilder dx)
 where
  unwrapBuilder ::
    TreeVecBuilder (ReprGrad leaf) ('TreeShapeLeaf leafSh) ->
    VecBuilder (ReprGrad leaf @@ leafSh)
  unwrapBuilder (TreeVecBuilder builder) = builder

unwrapBVarLeafTree ::
  forall r leaf leafSh.
  (Representation.HasGradRepr leaf) =>
  Sing leafSh ->
  BVar r (Tree leaf ('TreeShapeLeaf leafSh)) ->
  BVar r (leaf @@ leafSh)
unwrapBVarLeafTree leafSh (BVar (Tree x) dx) = case Representation.hasGradReprDict @leaf leafSh of
  Dict -> BVar x (inlineNode unTree TreeVecBuilder dx)

projectBVarTreeField ::
  forall r record leaf childSh.
  (IsRecord record, Representation.HasGradRepr leaf) =>
  FieldGetter record childSh ->
  BVar r (Tree leaf ('TreeShapeRecord record)) ->
  BVar r (Tree leaf childSh)
projectBVarTreeField getter (BVar x dx) = withSingI (recordFieldSh getter) $
  BVar (projectValue x) (projectNode projectTangent injectGradient dx)
 where
  projectValue :: Tree leaf ('TreeShapeRecord record) -> Tree leaf childSh
  projectValue (Tree fields) = Tree (recordField getter fields)

  projectTangent ::
    Tree (ReprTang leaf) ('TreeShapeRecord record) ->
    Tree (ReprTang leaf) childSh
  projectTangent (Tree fields) = Tree (recordField getter fields)

  injectGradient ::
    TreeVecBuilder (ReprGrad leaf) childSh ->
    TreeVecBuilder (ReprGrad leaf) ('TreeShapeRecord record)
  injectGradient childBuilder =
    TreeVecBuilder (treeFieldBuilder getter childBuilder)

splitBVarTree ::
  forall r leaf sh.
  (BVarRepr leaf, Representation.HasGradRepr leaf) =>
  Sing sh ->
  BVar r (Tree leaf sh) ->
  Tree (ReprBVar r leaf) sh
splitBVarTree (STreeLeaf leafSh) x =
  Tree (splitBVarField @leaf leafSh (unwrapBVarLeafTree leafSh x))
splitBVarTree (STreeRecord (SShR @record)) x =
  Tree (mapRecordEx splitChild (recordGettersEx @record))
 where
  splitChild ::
    forall childSh.
    Sing childSh ->
    FieldGetter record childSh ->
    TreeOf (ReprBVar r leaf) @@ childSh
  splitChild childSh getter =
    unTree (splitBVarTree @r @leaf childSh (projectBVarTreeField getter x))

joinTreeRecordBVar ::
  forall r record leaf.
  (BVarRepr leaf, Representation.HasGradRepr leaf, IsRecord record) =>
  record (TreeOf (ReprBVar r leaf)) ->
  BVar r (Tree leaf ('TreeShapeRecord record))
joinTreeRecordBVar fields = BVar value derivative
 where
  joinedFields :: record (TreeBVarFields r leaf)
  joinedFields = mapRecordEx joinChild fields

  joinChild ::
    forall childSh.
    Sing childSh ->
    TreeOf (ReprBVar r leaf) @@ childSh ->
    BVar r (Tree leaf childSh)
  joinChild childSh child = joinBVarTree @r @leaf childSh (Tree child)

  value :: Tree leaf ('TreeShapeRecord record)
  value = Tree (mapRecordEx (\_ -> unTree . bvarValue) joinedFields)

  derivative :: AD r (Tree (ReprTang leaf) ('TreeShapeRecord record))
  derivative = realNode (ExprSum (toList (foldZipRecordEx fieldTerms joinedFields (recordGettersEx @record))))

  fieldTerms ::
    forall childSh.
    Sing childSh ->
    BVar r (Tree leaf childSh) ->
    FieldGetter record childSh ->
    DList
      ( Term
          ('VecPair r (DualOf r))
          ( 'VecPair
              (Tree (ReprTang leaf) ('TreeShapeRecord record))
              (Tree (ReprGrad leaf) ('TreeShapeRecord record))
          )
      )
  fieldTerms childSh (BVar _ (AD childDerivative)) getter =
    DList.singleton (childDerivative (BiFun injectTangent projectGradient))
   where
    injectTangent ::
      Tree (ReprTang leaf) childSh ->
      TreeVecBuilder (ReprTang leaf) ('TreeShapeRecord record)
    injectTangent child =
      withSingI childSh $
        TreeVecBuilder
          ( treeFieldBuilder getter
              (identityBuilder @(Tree (ReprTang leaf) childSh) child)
          )

    projectGradient ::
      Tree (ReprGrad leaf) ('TreeShapeRecord record) ->
      TreeVecBuilder (ReprGrad leaf) childSh
    projectGradient (Tree gradients) =
      withSingI childSh $
        identityBuilder @(Tree (ReprGrad leaf) childSh)
          (Tree (recordField getter gradients))

joinBVarTree ::
  forall r leaf sh.
  (BVarRepr leaf, Representation.HasGradRepr leaf) =>
  Sing sh ->
  Tree (ReprBVar r leaf) sh ->
  BVar r (Tree leaf sh)
joinBVarTree (STreeLeaf leafSh) x =
  wrapBVarLeafTree leafSh (joinBVarField @leaf leafSh (unTree x))
joinBVarTree (STreeRecord SShR) x = joinTreeRecordBVar (unTree x)

instance
  (BVarRepr leaf, Representation.HasGradRepr leaf) =>
  BVarRepr (TyCon1 (Tree leaf))
 where
  type ReprBVar r (TyCon1 (Tree leaf)) = TyCon1 (Tree (ReprBVar r leaf))
  splitBVarField = splitBVarTree
  joinBVarField = joinBVarTree

-- | Differentiate a function written against a tree of variable leaves.
--
-- The tree may be rooted at either a leaf or a record. Its structure is split
-- recursively, while the leaf representation's 'BVarRepr' instance decides
-- how each leaf is exposed to the function.
adTree ::
  forall {k} (leaf :: k ~> Type) sh y.
  ( BVarRepr leaf
  , Representation.HasGradRepr leaf
  , SingI sh
  , HasBuilder (Tang y)
  , HasBuilder (Grad y)
  , DualOf (DualOf (Tang y)) ~ Tang y
  ) =>
  (forall r. Tree (ReprBVar r leaf) sh -> BVar r y) ->
  DFun (Tree leaf sh) y
adTree f = BVar.ad (f . splitBVarField @(TyCon1 (Tree leaf)) (sing @sh))

-- | Differentiate a function written against a top-level record of variable
-- leaves.
--
-- This is 'adTree' specialized to a record root. The callback receives the
-- ordinary record value rather than its enclosing 'Tree'.
adTreeRecord ::
  forall {k} (leaf :: k ~> Type) record y.
  ( BVarRepr leaf
  , Representation.HasGradRepr leaf
  , IsRecord record
  , HasBuilder (Tang y)
  , HasBuilder (Grad y)
  , DualOf (DualOf (Tang y)) ~ Tang y
  ) =>
  (forall r. record (TreeOf (ReprBVar r leaf)) -> BVar r y) ->
  DFun (Tree leaf ('TreeShapeRecord record)) y
adTreeRecord f = adTree (f . unTree)

type instance ReprScalar (TreeOf leaf) = ReprScalar leaf
type instance ReprDualOf (TreeOf leaf) = TreeOf (ReprDualOf leaf)

type instance ReprTang (TreeOf leaf) = TreeOf (ReprTang leaf)
