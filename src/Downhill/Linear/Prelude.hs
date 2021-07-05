{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Linear.Prelude
  ( pattern T2,
    pattern T3,
  )
where

import Downhill.Linear.BackGrad (BackGrad, DualBuilder, HasDual (DualOf), SparseGrad)
import Downhill.Linear.Expr (BasicVector, SparseVector (SparseVector, unSparseVector), maybeToMonoid)
import qualified Downhill.Linear.Lift as Lift
import qualified Downhill.Linear.Lift as Linear
import Prelude (Maybe (Just), Monoid (mempty), fmap, (.))
import qualified Prelude

toPair :: forall r a b. (HasDual a, HasDual b) => BackGrad r (a, b) -> (BackGrad r a, BackGrad r b)
toPair x = (Linear.lift1_sparse go1 x, Lift.lift1 (Lift.LinFun1 go2) x)
  where
    go1 :: SparseGrad a -> DualBuilder (a, b)
    go2 :: SparseGrad b -> DualBuilder (a, b)
    go1 (SparseVector da) = Just (da, mempty)
    go2 (SparseVector db) = Just (mempty, db)

toTriple ::
  forall r a b c.
  (BasicVector (DualOf a), BasicVector (DualOf b), BasicVector (DualOf c)) =>
  BackGrad r (a, b, c) ->
  (BackGrad r a, BackGrad r b, BackGrad r c)
toTriple x = (Lift.lift1 (Lift.LinFun1 go1) x, Lift.lift1 (Lift.LinFun1 go2) x, Lift.lift1 (Lift.LinFun1 go3) x)
  where
    go1 :: SparseGrad a -> DualBuilder (a, b, c)
    go2 :: SparseGrad b -> DualBuilder (a, b, c)
    go3 :: SparseGrad c -> DualBuilder (a, b, c)
    go1 (SparseVector da) = Just (da, mempty, mempty)
    go2 (SparseVector db) = Just (mempty, db, mempty)
    go3 (SparseVector dc) = Just (mempty, mempty, dc)

-- |
--
-- @
-- getFst :: (BasicVector (DualOf a), BasicVector (DualOf b)) => BackGrad r (a, b) -> BackGrad r a
-- getFst (T2 x _) = x
-- @
--
-- @
-- mkPair :: (BasicVector (DualOf a), BasicVector (DualOf b)) => BackGrad r a -> BackGrad r b -> BackGrad r (a, b)
-- mkPair x y = (T2 x y)
-- @
{-# COMPLETE T2 #-}

pattern T2 :: forall r a b. (HasDual a, HasDual b) => BackGrad r a -> BackGrad r b -> BackGrad r (a, b)
pattern T2 a b <-
  (toPair -> (a, b))
  where
    T2 = Lift.lift2 (Lift.LinFun2 go1 go2)
      where
        go1 :: SparseGrad (a, b) -> DualBuilder a
        go2 :: SparseGrad (a, b) -> DualBuilder b
        go1 = maybeToMonoid . fmap Prelude.fst . unSparseVector
        go2 = maybeToMonoid . fmap Prelude.snd . unSparseVector

{-# COMPLETE T3 #-}

pattern T3 ::
  forall r a b c.
  (BasicVector (DualOf a), BasicVector (DualOf b), BasicVector (DualOf c)) =>
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r (a, b, c)
pattern T3 a b c <-
  (toTriple -> (a, b, c))
  where
    T3 = Lift.lift3 (Lift.LinFun3 go1 go2 go3)
      where
        go1 :: SparseGrad (a, b, c) -> DualBuilder a
        go2 :: SparseGrad (a, b, c) -> DualBuilder b
        go3 :: SparseGrad (a, b, c) -> DualBuilder c
        go1 = maybeToMonoid . fmap (\(x, _, _) -> x) . unSparseVector
        go2 = maybeToMonoid . fmap (\(_, x, _) -> x) . unSparseVector
        go3 = maybeToMonoid . fmap (\(_, _, x) -> x) . unSparseVector
