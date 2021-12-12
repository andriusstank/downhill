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

import Downhill.Linear.BackGrad (BackGrad)
import Downhill.Linear.Expr (BasicVector (VecBuilder), maybeToMonoid)
import qualified Downhill.Linear.Lift as Lift
import Prelude (Maybe (Just), Monoid (mempty), fmap, (.))
import qualified Prelude

splitPair :: forall r a b. (BasicVector a, BasicVector b) => BackGrad r (a, b) -> (BackGrad r a, BackGrad r b)
splitPair x = (bg1, bg2)
  where
    go1 :: VecBuilder a -> VecBuilder (a, b)
    go2 :: VecBuilder b -> VecBuilder (a, b)
    go1 da = Just (da, mempty)
    go2 db = Just (mempty, db)
    bg1 :: BackGrad r a
    bg2 :: BackGrad r b
    bg1 = Lift.lift1_sparse go1 x
    bg2 = Lift.lift1_sparse go2 x

toTriple ::
  forall r a b c.
  (BasicVector a, BasicVector b, BasicVector c) =>
  BackGrad r (a, b, c) ->
  (BackGrad r a, BackGrad r b, BackGrad r c)
toTriple x = (bg1, bg2, bg3)
  where
    go1 :: VecBuilder a -> VecBuilder (a, b, c)
    go2 :: VecBuilder b -> VecBuilder (a, b, c)
    go3 :: VecBuilder c -> VecBuilder (a, b, c)
    go1 da = Just (da, mempty, mempty)
    go2 db = Just (mempty, db, mempty)
    go3 dc = Just (mempty, mempty, dc)
    bg1 :: BackGrad r a
    bg2 :: BackGrad r b
    bg3 :: BackGrad r c
    bg1 = Lift.lift1_sparse go1 x
    bg2 = Lift.lift1_sparse go2 x
    bg3 = Lift.lift1_sparse go3 x

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

pattern T2 :: forall r a b. (BasicVector a, BasicVector b) => BackGrad r a -> BackGrad r b -> BackGrad r (a, b)
pattern T2 a b <-
  (splitPair -> (a, b))
  where
    T2 a b = Lift.lift2_sparse go1 go2 a b
      where
        go1 :: VecBuilder (a, b) -> VecBuilder a
        go2 :: VecBuilder (a, b) -> VecBuilder b
        go1 = maybeToMonoid . fmap Prelude.fst
        go2 = maybeToMonoid . fmap Prelude.snd

{-# COMPLETE T3 #-}

pattern T3 ::
  forall r a b c.
  (BasicVector a, BasicVector b, BasicVector c) =>
  BackGrad r a ->
  BackGrad r b ->
  BackGrad r c ->
  BackGrad r (a, b, c)
pattern T3 a b c <-
  (toTriple -> (a, b, c))
  where
    T3 a b c = Lift.lift3_sparse go1 go2 go3 a b c
      where
        go1 :: VecBuilder (a, b, c) -> VecBuilder a
        go2 :: VecBuilder (a, b, c) -> VecBuilder b
        go3 :: VecBuilder (a, b, c) -> VecBuilder c
        go1 = maybeToMonoid . fmap (\(x, _, _) -> x)
        go2 = maybeToMonoid . fmap (\(_, x, _) -> x)
        go3 = maybeToMonoid . fmap (\(_, _, x) -> x)
