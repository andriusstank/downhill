{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.Tree (
    TreeShape (..),
    fillTreeI,
    mapTreeI,
    fillTreeEx,
    mapTreeEx,
)
where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, Sing, SingI (sing), withSingI, type (@@), type (~>))
import Downhill.Tree.FieldWrapper (FieldWrapper (..))
import Downhill.Tree.Record (IsRecord (..))
import NumHask.Prelude

data TreeShape leafShape = TreeShapeLeaf leafShape | TreeShapeRecord ((TreeShape leafShape ~> Type) -> Type)

data STreeShape (field :: TreeShape leafShape) where
    STreeLeaf :: Sing sh -> STreeShape ('TreeShapeLeaf sh)
    STreeRecord :: Dict (IsRecord subtree) -> STreeShape ('TreeShapeRecord subtree)

type instance Sing = STreeShape

instance (SingI sh) => SingI ('TreeShapeLeaf sh) where
    sing = STreeLeaf sing

instance (IsRecord subtree) => SingI ('TreeShapeRecord subtree) where
    sing = STreeRecord Dict

data TreeOf (leaf :: leafShape ~> Type) :: TreeShape leafShape ~> Type
type instance Apply (TreeOf leaf) ('TreeShapeLeaf sh) = leaf @@ sh
type instance Apply (TreeOf leaf) ('TreeShapeRecord subtree) = subtree (TreeOf leaf)

fillTreeEx :: forall a sh. (forall sh'. Sing sh' -> a @@ sh') -> Sing sh -> TreeOf a @@ sh
fillTreeEx f = go
  where
    go :: forall sh'. Sing sh' -> TreeOf a @@ sh'
    go (STreeLeaf sh) = f sh
    go (STreeRecord Dict) = fillRecordEx go

fillTreeI :: forall sh leaf. (SingI sh) => (forall sh1. (SingI sh1) => FieldWrapper leaf sh1) -> FieldWrapper (TreeOf leaf) sh
fillTreeI x = FieldWrapper (fillTreeEx @leaf @sh go sing)
  where
    go :: forall sh'. Sing sh' -> leaf @@ sh'
    go sh = withSingI sh (unFieldWrapper (x @sh'))

mapTreeEx :: forall a b sh. (forall sh'. Sing sh' -> a @@ sh' -> b @@ sh') -> Sing sh -> TreeOf a @@ sh -> TreeOf b @@ sh
mapTreeEx f = go
  where
    go :: forall sh'. Sing sh' -> TreeOf a @@ sh' -> TreeOf b @@ sh'
    go (STreeLeaf sh) = f sh
    go (STreeRecord Dict) = mapRecordEx go

mapTreeI ::
    forall sh a b.
    (SingI sh) =>
    (forall sh1. (SingI sh1) => FieldWrapper a sh1 -> FieldWrapper b sh1) ->
    FieldWrapper (TreeOf a) sh ->
    FieldWrapper (TreeOf b) sh
mapTreeI f (FieldWrapper x) = FieldWrapper (mapTreeEx @a @b @sh go sing x)
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
    go (STreeRecord Dict) = zipRecordEx go

zipTreeI ::
    forall sh a b c.
    (SingI sh) =>
    (forall sh'. (SingI sh') => FieldWrapper a sh' -> FieldWrapper b sh' -> FieldWrapper c sh') ->
    FieldWrapper (TreeOf a) sh ->
    FieldWrapper (TreeOf b) sh ->
    FieldWrapper (TreeOf c) sh
zipTreeI f (FieldWrapper x) (FieldWrapper y) = FieldWrapper (zipTreeEx @a @b @c @sh go sing x y)
  where
    go :: forall sh'. Sing sh' -> a @@ sh' -> b @@ sh' -> c @@ sh'
    go sh x' y' = withSingI sh (unFieldWrapper (f @sh' (FieldWrapper x') (FieldWrapper y')))

instance (SingI sh, forall sh1. (SingI sh1) => Additive (FieldWrapper leaf sh1)) => Additive (FieldWrapper (TreeOf leaf) sh) where
    zero = fillTreeI zero
    (+) = zipTreeI (+)
