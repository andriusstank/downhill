{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Opt.GD (AccGdProblem (..), accGd)
where

import Data.Stream.Infinite (Stream (..))
import Data.Stream.Infinite qualified as Stream
import Downhill.BVar (DFun (DFun))
import Downhill.Linear.Fun (DualOf, Metric, (|*>))
import Downhill.Manifold (Grad, MScalar, Manifold (..), Tang)
import NumHask.Prelude

gdStep ::
  forall p z g.
  (Manifold p, Metric g (Grad p), Tang z ~ z, DualOf (DualOf z) ~ z) =>
  DualOf z ->
  DFun p z ->
  g ->
  p ->
  (p, z)
gdStep dy (DFun loss) metric x =
  (x .-~^ step, y)
 where
  y :: z
  (y, f) = loss x
  step :: Tang p
  step = dy |*> f |*> metric

data GDState p = GDState
  { x :: p
  , v :: Tang p
  }

gdmStep ::
  forall p z g.
  (Manifold p, Metric g (Grad p), Tang z ~ z, DualOf (DualOf z) ~ z) =>
  DualOf z -> MScalar p -> DFun p z -> g -> GDState p -> (GDState p, z)
gdmStep dy momentum (DFun loss) metric state = (GDState x2 v2, y)
 where
  y :: z
  (y, f) = loss state.x
  step :: Tang p
  step = dy |*> f |*> metric
  x2 = state.x .-~^ v2
  v2 = momentum *| state.v + (one - momentum) *| step

data AccGdProblem p y g = AccGdProblem
  { loss :: DFun p y
  , metric :: g
  , momentum :: Maybe (MScalar p)
  , dy :: DualOf y
  }

accGd :: forall p y g. (Manifold p, Metric g (Grad p), Tang y ~ y, DualOf (DualOf y) ~ y) => AccGdProblem p y g -> p -> Stream (p, y)
accGd (AccGdProblem loss metric (Just momentum) dy) x0 = Stream.unfold step (GDState x0 zero)
 where
  step :: GDState p -> ((p, y), GDState p)
  step state = ((newState.x, y), newState)
   where
    (newState, y) = gdmStep dy momentum loss metric state
accGd (AccGdProblem loss metric Nothing dy) x0 = Stream.unfold step x0
 where
  step :: p -> ((p, y), p)
  step x = ((x, y), x1)
   where
    (x1, y) = gdStep dy loss metric x
