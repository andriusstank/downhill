{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | While 'BackGrad' is intended to be simple to construct manually, this module provides a way to do
--   that with a bit less of boilerplate.
module Downhill.Linear.Lift
  ( -- * @LinFunN@

    -- | A linear function of type @a -> b -> ... -> z@. Type of gradient of @z@ is @x@, which might
    -- be @DualOf z@ or some other type with compatible @VecBuilder@. It containts @BasicVector@ instance that tells
    -- how gradients should be accumulated, a proof that gradient builder type is right and a set of functions which
    -- propagate gradients to each of the parent nodes.
    LinFun1 (..),
    LinFun2 (..),
    LinFun3 (..),
    lift1,
    lift2,
    lift3,
    -- | Special cases. Not sure if they are really useful.
    lift1',
    lift1_dense,
    lift1_sparse,

    -- * @EasyFunN@

    -- | @EasyFunN@ doesn\'t even have a type for the gradient. @fromEasyN@ will invent an ad-hoc type and
    -- convert it to @LinFunN@. It's a bit hacky, but arguably easier way to use @BackGrad@s.
    EasyFun1 (..),
    EasyFun2 (..),
    EasyFun3 (..),
    fromEasy1,
    fromEasy2,
    fromEasy3,
    easyLift1,
    easyLift2,
    easyLift3,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Reflection (Reifies (reflect), reify)
import Downhill.Linear.BackGrad (BackGrad (..), DualBuilder, HasDual (DualOf), SparseGrad, castNode)
import Downhill.Linear.Expr (BasicVector (..), Expr (ExprSum))
import Prelude hiding (fst, snd, zip)

data LinFun3 a b c z where
  LinFun3 ::
    forall x a b c z.
    (BasicVector x, VecBuilder x ~ VecBuilder z) =>
    (x -> VecBuilder a) ->
    (x -> VecBuilder b) ->
    (x -> VecBuilder c) ->
    LinFun3 a b c z

data LinFun2 a b z where
  LinFun2 ::
    forall x a b z.
    (BasicVector x, VecBuilder x ~ VecBuilder z) =>
    (x -> VecBuilder a) ->
    (x -> VecBuilder b) ->
    LinFun2 a b z

data LinFun1 a z where
  LinFun1 ::
    forall x a z.
    (BasicVector x, VecBuilder x ~ VecBuilder z) =>
    (x -> VecBuilder a) ->
    LinFun1 a z

lift3 ::
  forall r a b c z.
  LinFun3 (DualOf a) (DualOf b) (DualOf c) (DualOf z) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r z
lift3 (LinFun3 fa fb fc) (BackGrad da) (BackGrad db) (BackGrad dc) = castNode node
  where
    node = ExprSum (da fa ++ db fb ++ dc fc)

lift2 ::
  forall r a b z.
  LinFun2 (DualOf a) (DualOf b) (DualOf z) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r z
lift2 (LinFun2 fa fb) (BackGrad da) (BackGrad db) = castNode node
  where
    node = ExprSum (da fa ++ db fb)

lift1 ::
  forall r a z.
  LinFun1 (DualOf a) (DualOf z) ->
  BackGrad r a ->
  BackGrad r z
lift1 (LinFun1 fa) (BackGrad da) = castNode node
  where
    node = ExprSum (da fa)

lift1' ::
  forall x r a z.
  (BasicVector x, VecBuilder x ~ DualBuilder z) =>
  (x -> DualBuilder a) ->
  BackGrad r a ->
  BackGrad r z
lift1' fa (BackGrad da) = castNode node
  where
    node = ExprSum (da fa)

lift1_dense ::
  forall r a z.
  HasDual z =>
  (DualOf z -> DualBuilder a) ->
  BackGrad r a ->
  BackGrad r z
lift1_dense = lift1'

lift1_sparse ::
  forall r a z.
  HasDual z =>
  (SparseGrad z -> DualBuilder a) ->
  BackGrad r a ->
  BackGrad r z
lift1_sparse = lift1'

newtype EasyFun3 a b c z = EasyFun3 (DualOf z -> (DualBuilder a, DualBuilder b, DualBuilder c))

newtype EasyFun2 a b z = EasyFun2 (DualOf z -> (DualBuilder a, DualBuilder b))

newtype EasyFun1 a z = EasyFun1 (DualOf z -> DualBuilder a)

-- The trick is to invoke EasyFun in sumBuilder. Normally data flow looks like this:
--
--               sumBuilder           +-- df computes gradients
--                  |                 v
--                  v          /---> df[a] -> DualBuilder a
-- (DualBuilder z) ---> (z)---+----> df[b] -> DualBuilder b
--                       ^     \---> df[c] -> DualBuilder c
--                       |
--                     Node
--
-- Sparse gradients `DualBuilder z` are summed and converted to dense gradient `z`. Linear function df
-- takes gradient z and produces sparse gradients (DualBuilder a, ...) which are then forwaded to
-- parents.
--
-- Gradient z is stored in the node, then for each outgoing edge corresponding part of function `df z`
-- is evaluated.

-- EasyFun is different. We have no z!
--
--              sumBuilder with df sneaked in
--                  |
--                  v                                                      /---> fst --> GradBulder a
-- (DualBuilder z) ----> (DualBuilder a, DualBuilder b, DualBuilder c) ---+----> snd --> GradBulder b
--                        \-----------------------------------------/      \---> thd --> GradBulder c
--                                           |
--                                          Node
--
-- We use BuilderTupleN as a type of the gradient of z, pretending it is a dense vector, with a
-- builder type of `DualBuilder z`. Type parameter s stores function we are lifting `df`.
--
-- Function df is given sparse gradients (presumably summing them by itself) and the result is stored in the node.
-- Then for each outgoing edge corresponding part of the result is looked up.
--
-- This is ugly, because sumBuilder is expected to preserve the value of the vector, instead of
-- covertly applying arbitrary functions. Also BuilderTupleN` is dishonest about it's type: it
-- advertises itself as a replacement of z, but it actually contains data of completely
-- unrelated types a, b, ...
-- However, we don't leak the type of node (BuilderTupleN) and no one can observe our shenanigans.

data BuilderTuple3 s a b c z = BuilderTuple3
  { b3get1 :: DualBuilder a,
    b3get2 :: DualBuilder b,
    b3get3 :: DualBuilder c
  }

instance (Reifies s (EasyFun3 a b c z), BasicVector (DualOf z)) => BasicVector (BuilderTuple3 s a b c z) where
  type VecBuilder (BuilderTuple3 s a b c z) = DualBuilder z
  sumBuilder zbs = wrap (f z)
    where
      z = sumBuilder zbs :: DualOf z
      wrap (a, b, c) = BuilderTuple3 a b c
      EasyFun3 f = reflect (Proxy @s) :: EasyFun3 a b c z

builder3Fun ::
  forall s a b c z.
  (Reifies s (EasyFun3 a b c z), BasicVector (DualOf z)) =>
  Proxy s ->
  LinFun3 (DualOf a) (DualOf b) (DualOf c) (DualOf z)
builder3Fun Proxy = LinFun3 @(BuilderTuple3 s a b c z) b3get1 b3get2 b3get3

fromEasy3 ::
  forall a b c z.
  BasicVector (DualOf z) =>
  EasyFun3 a b c z ->
  LinFun3 (DualOf a) (DualOf b) (DualOf c) (DualOf z)
fromEasy3 f = reify f builder3Fun

easyLift3 ::
  forall r a b c z.
  BasicVector (DualOf z) =>
  EasyFun3 a b c z ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r z
easyLift3 = lift3 . fromEasy3

data BuilderTuple2 s a b z = BuilderTuple2
  { b2get1 :: DualBuilder a,
    b2get2 :: DualBuilder b
  }

instance (Reifies s (EasyFun2 a b z), BasicVector (DualOf z)) => BasicVector (BuilderTuple2 s a b z) where
  type VecBuilder (BuilderTuple2 s a b z) = DualBuilder z
  sumBuilder zbs = wrap (f z)
    where
      z = sumBuilder zbs :: DualOf z
      wrap (a, b) = BuilderTuple2 a b
      EasyFun2 f = reflect (Proxy @s) :: EasyFun2 a b z

builder2Fun ::
  forall s a b z.
  (Reifies s (EasyFun2 a b z), BasicVector (DualOf z)) =>
  Proxy s ->
  LinFun2 (DualOf a) (DualOf b) (DualOf z)
builder2Fun Proxy = LinFun2 @(BuilderTuple2 s a b z) b2get1 b2get2

fromEasy2 ::
  forall a b z.
  BasicVector (DualOf z) =>
  EasyFun2 a b z ->
  LinFun2 (DualOf a) (DualOf b) (DualOf z)
fromEasy2 f = reify f builder2Fun

easyLift2 ::
  forall r a b z.
  BasicVector (DualOf z) =>
  EasyFun2 a b z ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r z
easyLift2 = lift2 . fromEasy2

newtype BuilderTuple1 s a z = BuilderTuple1 {b1get1 :: DualBuilder a}

instance (Reifies s (EasyFun1 a z), BasicVector (DualOf z)) => BasicVector (BuilderTuple1 s a z) where
  type VecBuilder (BuilderTuple1 s a z) = DualBuilder z
  sumBuilder zbs = wrap (f z)
    where
      z = sumBuilder zbs :: DualOf z
      wrap a = BuilderTuple1 a
      EasyFun1 f = reflect (Proxy @s) :: EasyFun1 a z

builder1Fun ::
  forall s a z.
  (Reifies s (EasyFun1 a z), BasicVector (DualOf z)) =>
  Proxy s ->
  LinFun1 (DualOf a) (DualOf z)
builder1Fun Proxy = LinFun1 @(BuilderTuple1 s a z) b1get1

fromEasy1 ::
  forall a z.
  BasicVector (DualOf z) =>
  EasyFun1 a z ->
  LinFun1 (DualOf a) (DualOf z)
fromEasy1 f = reify f builder1Fun

easyLift1 ::
  forall r a z.
  BasicVector (DualOf z) =>
  EasyFun1 a z ->
  BackGrad r a ->
  BackGrad r z
easyLift1 = lift1 . fromEasy1
