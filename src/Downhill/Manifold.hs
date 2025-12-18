{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Manifold (Manifold (..), MScalar, Grad, IsScalar, HasGrad)
where

import Downhill.Linear.Fun (DualOf)
import Downhill.VSpace (VSpace)
import NumHask.Prelude
import Downhill.Linear.Builder (HasBuilder)

type MScalar p = Scalar (Tang p)

class
  ( VSpace (Tang p)
  ) =>
  Manifold p
  where
  type Tang p :: Type

  (.+~^) :: p -> Tang p -> p
  (.-~^) :: p -> Tang p -> p

type Grad p = DualOf (Tang p)

type IsScalar a = (Manifold a, Tang a ~ a, Scalar a ~ a)
type HasGrad a = (Manifold a, HasBuilder (Tang a), HasBuilder (Grad a))

instance
  ( Manifold a
  , Manifold b
  , MScalar b ~ MScalar a
  ) =>
  Manifold (a, b)
  where
  type Tang (a, b) = (Tang a, Tang b)
  (x1, x2) .+~^ (y1, y2) = (x1 .+~^ y1, x2 .+~^ y2)
  (x1, x2) .-~^ (y1, y2) = (x1 .-~^ y1, x2 .-~^ y2)

instance
  ( Manifold a
  , Manifold b
  , Manifold c
  , MScalar b ~ MScalar a
  , MScalar c ~ MScalar a
  ) =>
  Manifold (a, b, c)
  where
  type Tang (a, b, c) = (Tang a, Tang b, Tang c)

  (x1, x2, x3) .+~^ (y1, y2, y3) = (x1 .+~^ y1, x2 .+~^ y2, x3 .+~^ y3)
  (x1, x2, x3) .-~^ (y1, y2, y3) = (x1 .-~^ y1, x2 .-~^ y2, x3 .-~^ y3)

instance Manifold Double where
  type Tang Double = Double
  (.+~^) = (+)
  (.-~^) = (-)
