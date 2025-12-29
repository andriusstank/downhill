{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Tree (
  TreeShape (..),
  TreeOf,
  fillTree,
  mapTree,
  zipTree,
  fillTreeEx,
  mapTreeEx,
  -- splitBVarTree,
  TreeBuilderType
)
where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, Sing, SingI (sing), TyCon, TyCon1, withSingI, type (@@), type (~>), withSing)
import Downhill.BVar (BVar (..))
import Downhill.Linear.AD (SparseFun (SparseFun), inlineNode, sparseNode)
import Downhill.Linear.Builder (HasBuilder (VecBuilder, identityBuilder, sumBuilder))
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..))
import Downhill.Tree.FieldWrapper (EqField, FieldDualOf, FieldScalar, FieldTang, FieldWrapper (..), HasBuilderField, ManifoldField, MultiplicativeActionField, VSpaceField, AdditiveField)
import Downhill.Tree.Record (IsRecord (..), Record, SShR (SShR), ShR (ShR), foldMapRecord, foldZipRecordEx, RecordBuilder)
import Downhill.VSpace (VSpace ((|*|)))
import NumHask.Prelude
import Prelude.Singletons (ConstSym0, ConstSym1)

data TreeShape leafShape = TreeShapeLeaf leafShape | TreeShapeRecord ((TreeShape leafShape ~> Type) -> Type)

data STreeShape (field :: TreeShape leafShape) where
  STreeLeaf :: Sing sh -> STreeShape ('TreeShapeLeaf sh)
  -- STreeRecord :: Dict (IsRecord subtree) -> STreeShape ('TreeShapeRecord subtree)
  STreeRecord :: Sing ('ShR subtree) -> STreeShape ('TreeShapeRecord subtree)

type instance Sing = STreeShape

instance (SingI sh) => SingI ('TreeShapeLeaf sh) where
  sing = STreeLeaf sing

instance (IsRecord subtree) => SingI ('TreeShapeRecord subtree) where
  sing = STreeRecord sing

data TreeOf (leaf :: leafShape ~> Type) :: TreeShape leafShape ~> Type
type instance Apply (TreeOf leaf) ('TreeShapeLeaf sh) = leaf @@ sh
type instance Apply (TreeOf leaf) ('TreeShapeRecord subtree) = subtree (TreeOf leaf)

fillTreeEx :: forall a sh. (forall sh'. Sing sh' -> a @@ sh') -> Sing sh -> TreeOf a @@ sh
fillTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf a @@ sh'
  go (STreeLeaf sh) = f sh
  go (STreeRecord SShR) = fillRecordEx go

fillTree :: forall sh leaf. (SingI sh) => (forall sh1. (SingI sh1) => FieldWrapper leaf sh1) -> FieldWrapper (TreeOf leaf) sh
fillTree x = FieldWrapper (fillTreeEx @leaf @sh go sing)
 where
  go :: forall sh'. Sing sh' -> leaf @@ sh'
  go sh = withSingI sh (unFieldWrapper (x @sh'))

mapTreeEx :: forall a b sh. (forall sh'. Sing sh' -> a @@ sh' -> b @@ sh') -> Sing sh -> TreeOf a @@ sh -> TreeOf b @@ sh
mapTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf a @@ sh' -> TreeOf b @@ sh'
  go (STreeLeaf sh) = f sh
  go (STreeRecord SShR) = mapRecordEx go

mapTree ::
  forall sh a b.
  (SingI sh) =>
  (forall sh1. (SingI sh1) => FieldWrapper a sh1 -> FieldWrapper b sh1) ->
  FieldWrapper (TreeOf a) sh ->
  FieldWrapper (TreeOf b) sh
mapTree f (FieldWrapper x) = FieldWrapper (mapTreeEx @a @b @sh go sing x)
 where
  go :: forall sh'. Sing sh' -> a @@ sh' -> b @@ sh'
  go sh x' = withSingI sh (unFieldWrapper (f @sh' (FieldWrapper x')))

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

zipTree ::
  forall sh a b c.
  (SingI sh) =>
  (forall sh'. (SingI sh') => FieldWrapper a sh' -> FieldWrapper b sh' -> FieldWrapper c sh') ->
  FieldWrapper (TreeOf a) sh ->
  FieldWrapper (TreeOf b) sh ->
  FieldWrapper (TreeOf c) sh
zipTree f (FieldWrapper x) (FieldWrapper y) = FieldWrapper (zipTreeEx @a @b @c @sh go sing x y)
 where
  go :: forall sh'. Sing sh' -> a @@ sh' -> b @@ sh' -> c @@ sh'
  go sh x' y' = withSingI sh (unFieldWrapper (f @sh' (FieldWrapper x') (FieldWrapper y')))

foldTreeEx :: forall a sh. (Monoid a) => Sing sh -> TreeOf (ConstSym1 a) @@ sh -> a
foldTreeEx (STreeLeaf _) x = x
foldTreeEx (STreeRecord SShR) x = foldMapRecordEx foldTreeEx x

foldTree ::
  forall sh a.
  (SingI sh, Monoid a) =>
  FieldWrapper (TreeOf (ConstSym1 a)) sh ->
  a
foldTree (FieldWrapper x) = foldTreeEx @a @sh sing x

sumTree ::
  forall sh a.
  (SingI sh, Additive a) =>
  FieldWrapper (TreeOf (ConstSym1 a)) sh ->
  a
sumTree = getSum . foldMapTree (Sum . unFieldWrapper)

foldMapTreeEx :: forall leaf a sh. (Monoid a) => (forall sh'. Sing sh' -> leaf @@ sh' -> a) -> Sing sh -> TreeOf leaf @@ sh -> a
foldMapTreeEx f = go
 where
  go :: forall sh'. Sing sh' -> TreeOf leaf @@ sh' -> a
  go (STreeLeaf sh) x = f sh x
  go (STreeRecord SShR) x = foldMapRecordEx go x

foldMapTree :: forall leaf a sh. (Monoid a, SingI sh) => (forall sh'. (SingI sh') => FieldWrapper leaf sh' -> a) -> FieldWrapper (TreeOf leaf) sh -> a
foldMapTree f (FieldWrapper x) = foldMapTreeEx @leaf f' (sing @sh) x
 where
  f' :: forall sh'. Sing sh' -> leaf @@ sh' -> a
  f' sh' y = withSingI sh' (f (FieldWrapper @leaf @sh' y))

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

foldZipTree ::
  forall a b m sh.
  (Monoid m, SingI sh) =>
  (forall sh'. (SingI sh') => FieldWrapper a sh' -> FieldWrapper b sh' -> m) ->
  FieldWrapper (TreeOf a) sh ->
  FieldWrapper (TreeOf b) sh ->
  m
foldZipTree f (FieldWrapper x) (FieldWrapper y) = foldZipTreeEx @a @b f' (sing @sh) x y
 where
  f' :: forall sh'. Sing sh' -> a @@ sh' -> b @@ sh' -> m
  f' sh' x' y' = withSingI sh' (f (FieldWrapper @a @sh' x') (FieldWrapper @b @sh' y'))

instance (SingI sh, forall sh1. (SingI sh1) => Additive (FieldWrapper leaf sh1)) => Additive (FieldWrapper (TreeOf leaf) sh) where
  zero = fillTree zero
  (+) = zipTree (+)

instance (SingI sh, forall sh1. (SingI sh1) => Subtractive (FieldWrapper leaf sh1)) => Subtractive (FieldWrapper (TreeOf leaf) sh) where
  negate = mapTree negate
  (-) = zipTree (-)

instance (SingI sh, forall sh1. (SingI sh1) => Multiplicative (FieldWrapper leaf sh1)) => Multiplicative (FieldWrapper (TreeOf leaf) sh) where
  one = fillTree one
  (*) = zipTree (*)

instance (MultiplicativeActionField leaf, SingI sh) => MultiplicativeAction (FieldWrapper (TreeOf leaf) sh) where
  type Scalar (FieldWrapper (TreeOf leaf) sh) = FieldScalar leaf
  v |* a = mapTree (|* a) v

type instance FieldScalar (TreeOf leaf) = FieldScalar leaf
type instance FieldDualOf (TreeOf leaf) = TreeOf (FieldDualOf leaf)

instance (VSpaceField leaf, SingI sh) => VSpace (FieldWrapper (TreeOf leaf) sh) where
  dx |*| x = getSum (foldZipTree mulLeaf dx x)
   where
    mulLeaf :: (SingI sh') => FieldWrapper (FieldDualOf leaf) sh' -> FieldWrapper leaf sh' -> Sum (FieldScalar leaf)
    mulLeaf dz z = Sum (dz |*| z)

type instance FieldTang (TreeOf leaf) = TreeOf (FieldTang leaf)

instance (ManifoldField leaf, SingI sh) => Manifold (FieldWrapper (TreeOf leaf) sh) where
  type Tang (FieldWrapper (TreeOf leaf) sh) = FieldWrapper (TreeOf (FieldTang leaf)) sh
  (.+~^) = zipTree (.+~^)

instance (SingI sh, EqField f) => Eq (FieldWrapper (TreeOf f) sh) where
  x == y = getAll (foldZipTree eqField x y)
   where
    eqField :: (SingI sh') => FieldWrapper f sh' -> FieldWrapper f sh' -> All
    eqField x' y' = All (x' == y')

type family TreeBuilderType leaf sh where
  TreeBuilderType leaf ('TreeShapeLeaf sh) = VecBuilder (FieldWrapper leaf sh)
  TreeBuilderType leaf ('TreeShapeRecord record) = VecBuilder (FieldWrapper (Record (TreeOf leaf)) ('ShR record))

newtype TreeBuilder leaf sh = TreeBuilder (TreeBuilderType leaf sh)

catTreeBuilder :: forall a sh. HasBuilderField a => Sing sh -> TreeBuilder a sh -> TreeBuilder a sh -> TreeBuilder a sh
catTreeBuilder sh (TreeBuilder x) (TreeBuilder y) = TreeBuilder (f x y)
  where
    f :: TreeBuilderType a sh -> TreeBuilderType a sh -> TreeBuilderType a sh
    f = case sh of
      STreeLeaf sh' -> withSingI sh' (<>)
      STreeRecord sh' -> withSingI sh' (<>)

instance (HasBuilderField a, SingI sh) => Semigroup (TreeBuilder a sh) where
  (<>) = catTreeBuilder sing

instance (HasBuilderField a, SingI sh) => Monoid (TreeBuilder a sh) where
  mempty = case sing @sh of
    STreeLeaf leafSh -> TreeBuilder (withSingI leafSh mempty)
    STreeRecord rSh -> TreeBuilder (withSingI rSh mempty)

rewrapLeaf :: FieldWrapper a leafSh -> FieldWrapper (TreeOf a) ('TreeShapeLeaf leafSh)
rewrapLeaf (FieldWrapper x) = FieldWrapper x

unrewrapLeaf :: FieldWrapper (TreeOf a) ('TreeShapeLeaf leafSh) -> FieldWrapper a leafSh
unrewrapLeaf (FieldWrapper x) = FieldWrapper x

rewrapSubtree :: FieldWrapper (Record (TreeOf a)) ('ShR r) -> FieldWrapper (TreeOf a) ('TreeShapeRecord r)
rewrapSubtree (FieldWrapper x) = FieldWrapper x

unrewrapSubtree :: FieldWrapper (TreeOf a) ('TreeShapeRecord r) -> FieldWrapper (Record (TreeOf a)) ('ShR r)
unrewrapSubtree (FieldWrapper x) = FieldWrapper x

sumBuilderEx :: HasBuilderField a => Sing sh -> TreeBuilder a sh -> FieldWrapper (TreeOf a) sh
sumBuilderEx (STreeLeaf sh) (TreeBuilder x) = rewrapLeaf (withSingI sh (sumBuilder x))
sumBuilderEx (STreeRecord sh) (TreeBuilder x) = rewrapSubtree (withSingI sh (sumBuilder x))

identityBuilderEx :: HasBuilderField a => Sing sh -> FieldWrapper (TreeOf a) sh -> TreeBuilder a sh
identityBuilderEx (STreeLeaf sh) x = TreeBuilder (withSingI sh (identityBuilder (unrewrapLeaf x)))
identityBuilderEx (STreeRecord sh) x = TreeBuilder (withSingI sh (identityBuilder (unrewrapSubtree x)))

instance (HasBuilderField a, SingI sh) => HasBuilder (FieldWrapper (TreeOf a) sh) where
  type VecBuilder (FieldWrapper (TreeOf a) sh) = TreeBuilder a sh
  sumBuilder = sumBuilderEx sing
  identityBuilder = identityBuilderEx sing


{-
splitBVarTree ::
  forall a b sh r.
  (SingI sh, ManifoldField a) =>
  (forall sh'. (SingI sh') => BVar r (FieldWrapper a sh') -> FieldWrapper b sh') ->
  BVar r (FieldWrapper (TreeOf a) sh) ->
  FieldWrapper (TreeOf b) sh
splitBVarTree f = go sing
 where
  fEx :: forall sh'. Sing sh' -> BVar r (FieldWrapper a sh') -> FieldWrapper b sh'
  fEx sh = withSingI sh (f @sh')
  splitLeaf :: forall leafSh. Sing leafSh -> BVar r (FieldWrapper (TreeOf a) ('TreeShapeLeaf leafSh)) -> FieldWrapper (TreeOf b) ('TreeShapeLeaf leafSh)
  splitLeaf sh (BVar (FieldWrapper x) dx) = FieldWrapper y
    where
      FieldWrapper y = fEx sh (BVar (FieldWrapper x) (sparseNode (SparseFun fwd back) dx))
      fwd :: FieldWrapper (TreeOf (FieldTang a)) (TreeShapeLeaf leafSh) -> VecBuilder (Tang (FieldWrapper a leafSh))
      fwd = identityBuilder
      back :: VecBuilder (DualOf (Tang (FieldWrapper a leafSh))) -> VecBuilder (FieldWrapper (TreeOf (FieldDualOf (FieldTang a))) (TreeShapeLeaf leafSh))
      back = ()
  go :: forall sh'. Sing sh' -> BVar r (FieldWrapper (TreeOf a) sh') -> FieldWrapper (TreeOf b) sh'
  go = \case
    STreeLeaf leafSh -> splitLeaf leafSh
    STreeRecord _ -> _
-}