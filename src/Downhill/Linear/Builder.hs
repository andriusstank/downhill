{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Linear.Builder (
  HasBuilder (..),
  Dense (..), DenseBuilder (..),
  Sparse(..)
) where

import Downhill.Orphans ()
import NumHask.Prelude
import Downhill.Linear.Fun (DualOf)

class (Additive v, Monoid (VecBuilder v)) => HasBuilder v where
  -- | @VecBuilder v@ is a sparse representation of vector @v@. Edges of a computational graph
  -- produce builders, which are then summed into vectors in nodes. Monoid operation '<>'
  -- means addition of vectors, but it doesn't need to compute the sum immediately - it
  -- might defer computation until 'sumBuilder' is evaluated.
  --
  -- @
  -- sumBuilder mempty = zeroV
  -- sumBuilder (x <> y) = sumBuilder x + sumBuilder y
  -- sumBuilder . identityBuilder = id
  -- @
  --
  -- 'mempty' must be cheap. '<>' must be O(1).
  type VecBuilder v :: Type

  sumBuilder :: VecBuilder v -> v
  identityBuilder :: v -> VecBuilder v

maybeToMonoid :: (Monoid m) => Maybe m -> m
maybeToMonoid = fromMaybe mempty

sumPair :: (HasBuilder a, HasBuilder b) => (VecBuilder a, VecBuilder b) -> (a, b)
sumPair (x, y) = (sumBuilder x, sumBuilder y)

identityPair :: (HasBuilder a, HasBuilder b) => (a, b) -> (VecBuilder a, VecBuilder b)
identityPair (x, y) = (identityBuilder x, identityBuilder y)

instance (HasBuilder a, HasBuilder b) => HasBuilder (a, b) where
  type VecBuilder (a, b) = (Maybe (Sum (a, b)), Maybe (VecBuilder a, VecBuilder b))
  sumBuilder (x, Nothing) = getSum (maybeToMonoid x)
  sumBuilder (Nothing, Just y) = sumPair y
  sumBuilder (Just x, Just z) = sumPair (identityPair (getSum x) <> z)
  identityBuilder x = (Just (Sum x), Nothing)

sumTriple :: (HasBuilder a, HasBuilder b, HasBuilder c) => (VecBuilder a, VecBuilder b, VecBuilder c) -> (a, b, c)
sumTriple (x, y, z) = (sumBuilder x, sumBuilder y, sumBuilder z)

identityTriple :: (HasBuilder a, HasBuilder b, HasBuilder c) => (a, b, c) -> (VecBuilder a, VecBuilder b, VecBuilder c)
identityTriple (x, y, z) = (identityBuilder x, identityBuilder y, identityBuilder z)

instance (HasBuilder a, HasBuilder b, HasBuilder c) => HasBuilder (a, b, c) where
  type VecBuilder (a, b, c) = (Maybe (Sum (a, b, c)), Maybe (VecBuilder a, VecBuilder b, VecBuilder c))
  sumBuilder (x, Nothing) = getSum (maybeToMonoid x)
  sumBuilder (Nothing, Just y) = sumTriple y
  sumBuilder (Just x, Just z) = sumTriple (identityTriple (getSum x) <> z)
  identityBuilder x = (Just (Sum x), Nothing)

newtype Dense a = Dense {unDense :: a}
  deriving (Additive, Subtractive, Multiplicative, Num, Eq) via a
  deriving Show

newtype DenseBuilder a = DenseBuilder {unDenseBuilder :: Maybe a}
  deriving (Semigroup, Monoid) via (Maybe (Sum a))

instance (Additive a) => HasBuilder (Dense a) where
  type VecBuilder (Dense a) = DenseBuilder a
  sumBuilder (DenseBuilder x )= Dense (fromMaybe zero x)
  identityBuilder (Dense v) = DenseBuilder (Just v)

type instance DualOf (Dense a) = Dense (DualOf a)

deriving via Dense Int instance HasBuilder Int
deriving via Dense Integer instance HasBuilder Integer
deriving via Dense Float instance HasBuilder Float
deriving via Dense Double instance HasBuilder Double


newtype Sparse v = Sparse
  { unSparse :: VecBuilder v}

deriving via (VecBuilder v) instance (Semigroup (VecBuilder v)) => Semigroup (Sparse v)

instance (Monoid (VecBuilder v)) => Additive (Sparse v) where
  zero = Sparse mempty
  Sparse x + Sparse y = Sparse (x <> y)

instance (Monoid (VecBuilder v)) => HasBuilder (Sparse v) where
  type VecBuilder (Sparse v) = VecBuilder v
  sumBuilder = Sparse
  identityBuilder (Sparse x) = x
