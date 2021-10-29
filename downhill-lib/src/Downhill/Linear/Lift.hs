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
  ( -- * Lifts
    lift1,
    lift2,
    lift3,

    -- * Dense lifts
    lift1_dense,
    lift2_dense,
    lift3_dense,

    -- * Lifts for 'SparseVector'
    lift1_sparse,
    lift2_sparse,
    lift3_sparse,
  )
where

import Downhill.Linear.BackGrad (BackGrad (..), castBackGrad, realNode)
import Downhill.Linear.Expr (BasicVector (..), Expr (ExprSum), FullVector (identityBuilder), SparseVector (unSparseVector))
import Prelude hiding (fst, snd, zip)

lift1 ::
  forall z r a.
  BasicVector z =>
  (z -> VecBuilder a) ->
  BackGrad r a ->
  BackGrad r z
lift1 fa (BackGrad da) = realNode node
  where
    node = ExprSum [da fa]

lift2 ::
  forall z r a b.
  BasicVector z =>
  (z -> VecBuilder a) ->
  (z -> VecBuilder b) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r z
lift2 fa fb (BackGrad da) (BackGrad db) = realNode node
  where
    node = ExprSum [da fa, db fb]

lift3 ::
  forall z r a b c.
  BasicVector z =>
  (z -> VecBuilder a) ->
  (z -> VecBuilder b) ->
  (z -> VecBuilder c) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r z
lift3 fa fb fc (BackGrad da) (BackGrad db) (BackGrad dc) = realNode node
  where
    node = ExprSum [da fa, db fb, dc fc]

lift1_sparse ::
  forall r a z.
  BasicVector z =>
  (VecBuilder z -> VecBuilder a) ->
  BackGrad r a ->
  BackGrad r z
lift1_sparse fa = castBackGrad . lift1 @(SparseVector z) fa'
  where
    fa' = fa . unSparseVector

lift2_sparse ::
  forall r a b z.
  BasicVector z =>
  (VecBuilder z -> VecBuilder a) ->
  (VecBuilder z -> VecBuilder b) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r z
lift2_sparse fa fb a b = castBackGrad $ lift2 @(SparseVector z) fa' fb' a b
  where
    fa' = fa . unSparseVector
    fb' = fb . unSparseVector

lift3_sparse ::
  forall r a b c z.
  BasicVector z =>
  (VecBuilder z -> VecBuilder a) ->
  (VecBuilder z -> VecBuilder b) ->
  (VecBuilder z -> VecBuilder c) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r z
lift3_sparse fa fb fc a b c =
  castBackGrad $
    lift3 @(SparseVector z) fa' fb' fc' a b c
  where
    fa' = fa . unSparseVector
    fb' = fb . unSparseVector
    fc' = fc . unSparseVector

lift1_dense ::
  (BasicVector v, FullVector a) =>
  ((v -> a) -> BackGrad r a -> BackGrad r v)
lift1_dense fa = lift1 (identityBuilder . fa)

lift2_dense ::
  (BasicVector v, FullVector a, FullVector b) =>
  (v -> a) ->
  (v -> b) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r v
lift2_dense fa fb = lift2 (identityBuilder . fa) (identityBuilder . fb)

lift3_dense ::
  (BasicVector v, FullVector a, FullVector b, FullVector c) =>
  (v -> a) ->
  (v -> b) ->
  (v -> c) ->
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r v
lift3_dense fa fb fc = lift3 (identityBuilder . fa) (identityBuilder . fb) (identityBuilder . fc)
