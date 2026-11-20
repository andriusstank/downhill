{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Linear.Fun (
  DualOf,
  LinearMap (..),
  Metric,
  lId,
  lFlip,
  lZero,
  lFst,
  lSnd,
  AsLinearMap (..),
  (|*>),
  (<*|),
)
where

import NumHask.Prelude

type family DualOf (v :: Type) :: Type

type instance DualOf Int = Int
type instance DualOf Integer = Integer
type instance DualOf Float = Float
type instance DualOf Double = Double
type instance DualOf () = ()
type instance DualOf (a, b) = (DualOf a, DualOf b)
type instance DualOf (a, b, c) = (DualOf a, DualOf b, DualOf c)
type instance DualOf (Generically v) = Generically (DualOf v)
type instance DualOf (Complex a) = Complex a
type instance DualOf (Ratio a) = Ratio a

-- | @LinearMap@ must obey the laws of @AsLinearMap@.
data LinearMap a b = LinearMap
  { lmapFwd :: a -> b
  , lmapBack :: DualOf b -> DualOf a
  }

instance Category LinearMap where
  id = LinearMap id id
  LinearMap f1 b1 . LinearMap f2 b2 = LinearMap (f1 . f2) (b2 . b1)

-- | (db |*> f) |*| a == db |*| f <*| a
class AsLinearMap f a b | f a -> b, f b -> a where
  toLinearMap :: f -> LinearMap a b

instance (a ~ a', b ~ b') => AsLinearMap (LinearMap a b) a' b' where
  toLinearMap = id


infixr 7 <*|
(<*|) :: (AsLinearMap f a b) => f -> a -> b
(<*|) = lmapFwd . toLinearMap

infixl 7 |*>
(|*>) ::
  forall f da db.
  ( AsLinearMap f (DualOf da) (DualOf db)
  , DualOf (DualOf da) ~ da
  , DualOf (DualOf db) ~ db
  ) =>
  db -> f -> da
(|*>) = flip (lmapBack . toLinearMap @f @(DualOf da) @(DualOf db))

-- | @Metric@ is a symmetric inverse metric mapping covectors to vectors.
class (AsLinearMap f a (DualOf a)) => Metric f a

lFlip :: (DualOf (DualOf a) ~ a, DualOf (DualOf b) ~ b) => LinearMap a b -> LinearMap (DualOf b) (DualOf a)
lFlip (LinearMap fwd back) = LinearMap back fwd

lId :: LinearMap a a
lId = LinearMap id id

lFst :: forall a b. (Additive (DualOf b)) => LinearMap (a, b) a
lFst = LinearMap fst (,zero)

lSnd :: forall a b. (Additive (DualOf a)) => LinearMap (a, b) b
lSnd = LinearMap snd (zero,)

lZero :: (Additive b, Additive (DualOf a)) => LinearMap a b
lZero = LinearMap (const zero) (const zero)
