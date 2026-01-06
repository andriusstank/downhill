{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}

module Downhill.Manifold (Tang, Manifold (..), MScalar, Grad, IsScalar, HasGrad)
where

import Downhill.Linear.Fun (DualOf)
import Downhill.VSpace (VSpace)
import NumHask.Prelude
import Downhill.Linear.Builder (HasBuilder)

type MScalar p = Scalar (Tang p)

type family Tang (p :: Type) :: Type
class
  ( VSpace (Tang p)
  , Tang (MScalar p) ~ MScalar p
  , Tang (Tang p) ~ Tang p
  , Tang (DualOf p) ~ DualOf p
  ) =>
  Manifold p
  where
  (.+~^) :: p -> Tang p -> p
  (.-~^) :: p -> Tang p -> p

type Grad p = DualOf (Tang p)

type IsScalar a = (Manifold a, Tang a ~ a, Scalar a ~ a)
type HasGrad a = (Manifold a, HasBuilder (Tang a), HasBuilder (Grad a))

type instance Tang Int = Int
instance Manifold Int where
  (.+~^) = (+)
  (.-~^) = (-)

type instance Tang Integer = Integer
instance Manifold Integer where
  (.+~^) = (+)
  (.-~^) = (-)

type instance Tang Float = Float
instance Manifold Float where
  (.+~^) = (+)
  (.-~^) = (-)

type instance Tang Double = Double
instance Manifold Double where
  (.+~^) = (+)
  (.-~^) = (-)


type instance Tang (a, b) = (Tang a, Tang b)
instance
  ( Manifold a
  , Manifold b
  , MScalar b ~ MScalar a
  ) =>
  Manifold (a, b)
  where
  (x1, x2) .+~^ (y1, y2) = (x1 .+~^ y1, x2 .+~^ y2)
  (x1, x2) .-~^ (y1, y2) = (x1 .-~^ y1, x2 .-~^ y2)

type instance Tang (a, b, c) = (Tang a, Tang b, Tang c)
instance
  ( Manifold a
  , Manifold b
  , Manifold c
  , MScalar b ~ MScalar a
  , MScalar c ~ MScalar a
  ) =>
  Manifold (a, b, c)
  where

  (x1, x2, x3) .+~^ (y1, y2, y3) = (x1 .+~^ y1, x2 .+~^ y2, x3 .+~^ y3)
  (x1, x2, x3) .-~^ (y1, y2, y3) = (x1 .-~^ y1, x2 .-~^ y2, x3 .-~^ y3)
