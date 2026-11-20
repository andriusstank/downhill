{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Downhill.Manifold (Tang, Manifold (..), MScalar, Grad, IsScalar, HasGrad)
where

import Downhill.Linear.Builder (HasBuilder, Dense)
import Downhill.Linear.Fun (DualOf)
import Downhill.Orphans ()
import Downhill.VSpace (VSpace, RealScalar)
import NumHask.Prelude

type MScalar p = Scalar (Tang p)

type family Tang (p :: Type) :: Type
class
  ( VSpace (Tang p)
  , Tang (MScalar p) ~ MScalar p
  , Tang (Tang p) ~ Tang p
  , Tang (Grad p) ~ Grad p
  ) =>
  Manifold p
  where
  (.+~^) :: p -> Tang p -> p
  default (.+~^) :: p ~ Tang p => p -> Tang p -> p
  (.+~^) = (+)
  (.-~^) :: p -> Tang p -> p
  default (.-~^) :: p ~ Tang p => p -> Tang p -> p
  (.-~^) = (-)

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

type instance Tang (Generically p) = Generically (Tang p)

type instance Tang (Complex a) = Complex a
instance RealScalar a => Manifold (Complex a) where
  (.+~^) = (+)
  (.-~^) = (-)

class GManifold f g where
  gmplus :: f p -> g p -> f p
  gmminus :: f p -> g p -> f p

instance (Manifold a, Tang a ~ b) => GManifold (K1 i a) (K1 i b) where
  gmplus (K1 x) (K1 y) = K1 (x .+~^ y)
  gmminus (K1 x) (K1 y) = K1 (x .-~^ y)

instance (GManifold f g) => GManifold (M1 i m f) (M1 i m g) where
  gmplus (M1 x) (M1 y) = M1 (gmplus x y)
  gmminus (M1 x) (M1 y) = M1 (gmminus x y)

instance (GManifold f1 g1, GManifold f2 g2) => GManifold (f1 :*: f2) (g1 :*: g2) where
  gmplus (x1 :*: x2) (y1 :*: y2) = gmplus x1 y1 :*: gmplus x2 y2
  gmminus (x1 :*: x2) (y1 :*: y2) = gmminus x1 y1 :*: gmminus x2 y2

instance GManifold U1 U1 where
  gmplus _ _ = U1
  gmminus _ _ = U1

instance
  ( Tang (Grad p) ~ Grad p
  , Tang (Scalar (Tang p)) ~ Scalar (Tang p)
  , Tang (Tang p) ~ Tang p
  , Tang (Scalar (Generically (Tang p))) ~ Scalar (Generically (Tang p))
  , VSpace (Generically (Tang p))
  , Generic p
  , Generic (Tang p)
  , GManifold (Rep p) (Rep (Tang p))
  ) =>
  Manifold (Generically p)
  where
  Generically x .+~^ Generically dx = Generically (to (from x `gmplus` from dx))
  Generically x .-~^ Generically dx = Generically (to (from x `gmminus` from dx))

type instance Tang (Dense v) = Dense (Tang v)

