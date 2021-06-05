{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Linear.Prelude
  ( fst,
    snd,
    zip,
    pattern T2,
    pattern T3,
  )
where

import Downhill.Linear.BackGrad (BackGrad, GradBuilder, HasGrad (GradOf), SparseGrad)
import Downhill.Linear.Expr (BasicVector, SparseVector (SparseVector, unSparseVector), maybeToMonoid)
import qualified Downhill.Linear.Lift as Lift
import Prelude (Maybe (Just), Monoid (mempty), fmap, (.))
import qualified Prelude

fst :: forall r a b. (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r (a, b) -> BackGrad r a
fst = Lift.lift1 (Lift.LinFun1 go)
  where
    go :: SparseGrad a -> GradBuilder (a, b)
    go (SparseVector da) = Just (da, mempty)

snd :: forall r a b. (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r (a, b) -> BackGrad r b
snd = Lift.lift1 (Lift.LinFun1 go)
  where
    go :: SparseGrad b -> GradBuilder (a, b)
    go (SparseVector db) = Just (mempty, db)

zip :: forall r a b. (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r a -> BackGrad r b -> BackGrad r (a, b)
zip = Lift.lift2 (Lift.LinFun2 go1 go2)
  where
    go1 :: SparseGrad (a, b) -> GradBuilder a
    go2 :: SparseGrad (a, b) -> GradBuilder b
    go1 = maybeToMonoid . fmap Prelude.fst . unSparseVector
    go2 = maybeToMonoid . fmap Prelude.snd . unSparseVector

toPair :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r (a, b) -> (BackGrad r a, BackGrad r b)
toPair x = (fst x, snd x)

fromPair :: forall r a b. (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r a -> BackGrad r b -> BackGrad r (a, b)
fromPair = Lift.lift2 (Lift.LinFun2 go1 go2)
  where
    go1 :: SparseGrad (a, b) -> GradBuilder a
    go2 :: SparseGrad (a, b) -> GradBuilder b
    go1 = maybeToMonoid . fmap Prelude.fst . unSparseVector
    go2 = maybeToMonoid . fmap Prelude.snd . unSparseVector

toTriple ::
  forall r a b c.
  (BasicVector (GradOf a), BasicVector (GradOf b), BasicVector (GradOf c)) =>
  BackGrad r (a, b, c) ->
  (BackGrad r a, BackGrad r b, BackGrad r c)
toTriple x = (Lift.lift1 (Lift.LinFun1 go1) x, Lift.lift1 (Lift.LinFun1 go2) x, Lift.lift1 (Lift.LinFun1 go3) x)
  where
    go1 :: SparseGrad a -> GradBuilder (a, b, c)
    go2 :: SparseGrad b -> GradBuilder (a, b, c)
    go3 :: SparseGrad c -> GradBuilder (a, b, c)
    go1 (SparseVector da) = Just (da, mempty, mempty)
    go2 (SparseVector db) = Just (mempty, db, mempty)
    go3 (SparseVector dc) = Just (mempty, mempty, dc)

fromTriple ::
  forall r a b c.
  (BasicVector (GradOf a), BasicVector (GradOf b), BasicVector (GradOf c)) =>
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r (a, b, c)
fromTriple = Lift.lift3 (Lift.LinFun3 go1 go2 go3)
  where
    go1 :: SparseGrad (a, b, c) -> GradBuilder a
    go2 :: SparseGrad (a, b, c) -> GradBuilder b
    go3 :: SparseGrad (a, b, c) -> GradBuilder c
    go1 = maybeToMonoid . fmap (\(x, _, _) -> x) . unSparseVector
    go2 = maybeToMonoid . fmap (\(_, x, _) -> x) . unSparseVector
    go3 = maybeToMonoid . fmap (\(_, _, x) -> x) . unSparseVector

-- |
--
-- @
-- getFst :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r (a, b) -> BackGrad r a
-- getFst (T2 x _) = x
-- @
--
-- @
-- mkPair :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r a -> BackGrad r b -> BackGrad r (a, b)
-- mkPair x y = (T2 x y)
-- @
{-# COMPLETE T2 #-}

pattern T2 :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BackGrad r a -> BackGrad r b -> BackGrad r (a, b)
pattern T2 a b <-
  (toPair -> (a, b))
  where
    T2 a b = fromPair a b

{-# COMPLETE T3 #-}

pattern T3 ::
  (BasicVector (GradOf a), BasicVector (GradOf b), BasicVector (GradOf c)) =>
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r (a, b, c)
pattern T3 a b c <-
  (toTriple -> (a, b, c))
  where
    T3 a b c = fromTriple a b c
