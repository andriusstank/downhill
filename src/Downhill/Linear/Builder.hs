{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Downhill.Linear.Builder (
  HasBuilder (..),
  DenseSparseBuilder (..),
  denseBuilder,
  sparseBuilder,
  sumDenseSparseBuilder,
  flattenDenseSparseBuilder,
  Dense (..),
  Sparse(..),
  maybeToMonoid,
  vectorElementBuilder,
) where

import Data.DList (DList)
import Data.DList qualified as DList
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Downhill.Orphans ()
import NumHask.Prelude
import Downhill.Linear.Fun (DualOf)
import GHC.TypeLits (KnownNat)

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
  -- Combining and evaluating builders should not repeatedly traverse or copy
  -- contributions that have already been accumulated.
  type VecBuilder v :: Type

  sumBuilder :: VecBuilder v -> v
  identityBuilder :: v -> VecBuilder v

maybeToMonoid :: (Monoid m) => Maybe m -> m
maybeToMonoid = fromMaybe mempty

-- | A builder with separate paths for complete values and sparse contributions.
--
-- The dense path makes 'identityBuilder' cheap: it can retain a complete value
-- without immediately decomposing it into sparse contributions.  The sparse
-- representation is chosen by the enclosing 'HasBuilder' instance.
data DenseSparseBuilder dense sparse = DenseSparseBuilder
  { densePart :: Maybe (Sum dense)
  , sparsePart :: Maybe sparse
  }

instance (Additive dense, Semigroup sparse) => Semigroup (DenseSparseBuilder dense sparse) where
  DenseSparseBuilder dense1 sparse1 <> DenseSparseBuilder dense2 sparse2 =
    DenseSparseBuilder (dense1 <> dense2) (sparse1 <> sparse2)

instance (Additive dense, Semigroup sparse) => Monoid (DenseSparseBuilder dense sparse) where
  mempty = DenseSparseBuilder mempty mempty

-- | Construct a builder contribution containing a complete value.
denseBuilder :: dense -> DenseSparseBuilder dense sparse
denseBuilder x = DenseSparseBuilder (Just (Sum x)) Nothing

-- | Construct a builder contribution containing only sparse data.
sparseBuilder :: sparse -> DenseSparseBuilder dense sparse
sparseBuilder x = DenseSparseBuilder Nothing (Just x)

flattenDenseSparseBuilder ::
    Monoid sparse =>
    (dense -> sparse) ->
    DenseSparseBuilder dense sparse ->
    sparse
flattenDenseSparseBuilder toSparse (DenseSparseBuilder dense sparse) = maybe mempty (toSparse . getSum) dense <> fromMaybe mempty sparse
  
sumDenseSparseBuilder :: (Additive dense, Semigroup sparse) => (dense -> sparse) -> (sparse -> dense) -> DenseSparseBuilder dense sparse -> dense
sumDenseSparseBuilder _toSparse _fromSparse (DenseSparseBuilder x Nothing) = getSum (maybeToMonoid x)
sumDenseSparseBuilder _toSparse fromSparse (DenseSparseBuilder Nothing (Just y)) = fromSparse y
sumDenseSparseBuilder toSparse fromSparse (DenseSparseBuilder (Just x) (Just y)) = fromSparse (toSparse (getSum x) <> y)

sumPair :: (HasBuilder a, HasBuilder b) => (VecBuilder a, VecBuilder b) -> (a, b)
sumPair (x, y) = (sumBuilder x, sumBuilder y)

identityPair :: (HasBuilder a, HasBuilder b) => (a, b) -> (VecBuilder a, VecBuilder b)
identityPair (x, y) = (identityBuilder x, identityBuilder y)

instance (HasBuilder a, HasBuilder b) => HasBuilder (a, b) where
  type VecBuilder (a, b) = DenseSparseBuilder (a, b) (VecBuilder a, VecBuilder b)
  sumBuilder = sumDenseSparseBuilder identityPair sumPair
  identityBuilder = denseBuilder

sumTriple :: (HasBuilder a, HasBuilder b, HasBuilder c) => (VecBuilder a, VecBuilder b, VecBuilder c) -> (a, b, c)
sumTriple (x, y, z) = (sumBuilder x, sumBuilder y, sumBuilder z)

identityTriple :: (HasBuilder a, HasBuilder b, HasBuilder c) => (a, b, c) -> (VecBuilder a, VecBuilder b, VecBuilder c)
identityTriple (x, y, z) = (identityBuilder x, identityBuilder y, identityBuilder z)

instance (HasBuilder a, HasBuilder b, HasBuilder c) => HasBuilder (a, b, c) where
  type VecBuilder (a, b, c) = DenseSparseBuilder (a, b, c) (VecBuilder a, VecBuilder b, VecBuilder c)
  sumBuilder = sumDenseSparseBuilder identityTriple sumTriple
  identityBuilder = denseBuilder

newtype Dense a = Dense {unDense :: a}
  deriving (Additive, Subtractive, Multiplicative, Num, Eq) via a
  deriving Show

instance MultiplicativeAction v => MultiplicativeAction (Dense v) where
  type Scalar (Dense v) = Dense (Scalar v)
  Dense x |* Dense y = Dense (x |* y)

--newtype DenseBuilder a = DenseBuilder {unDenseBuilder :: Maybe a}
--  deriving (Semigroup, Monoid) via (Maybe (Sum a))

instance (Additive a) => HasBuilder (Dense a) where
  type VecBuilder (Dense a) = Maybe (Sum a)
  --sumBuilder x = Dense (fromMaybe zero (_ x))
  sumBuilder = Dense . getSum . fromMaybe zero
  identityBuilder = Just . Sum . unDense

type instance DualOf (Dense a) = Dense (DualOf a)

deriving via Dense Int instance HasBuilder Int
deriving via Dense Integer instance HasBuilder Integer
deriving via Dense Float instance HasBuilder Float
deriving via Dense Double instance HasBuilder Double
deriving via Dense (Complex a) instance Additive a => HasBuilder (Complex a)

data VectorFieldBuilder n a = VectorFieldBuilder (Finite n) (VecBuilder a)

vectorElementBuilder :: Finite n -> VecBuilder a -> VecBuilder (Vector n a)
vectorElementBuilder index builder = (Nothing, DList.singleton (VectorFieldBuilder index builder))

type VectorBuilder n a = (Maybe (Sum (Vector n a)), DList (VectorFieldBuilder n a))

sumVectorBuilder :: forall n a. (KnownNat n, HasBuilder a) => VectorBuilder n a -> Vector n a
sumVectorBuilder (densePart, fields) = case DList.toList fields of
  [] -> denseVector
  sparseFields -> Vector.map (sumBuilder @a) accumulated
    where
      accumulated :: Vector n (VecBuilder a)
      accumulated =
        Vector.accum ((<>) @(VecBuilder a)) (Vector.map (identityBuilder @a) denseVector) (fieldBuilderPair <$> sparseFields)
  where
    denseVector = getSum (maybeToMonoid densePart)

    fieldBuilderPair (VectorFieldBuilder index fieldBuilder) = (index, fieldBuilder)

instance (KnownNat n, HasBuilder a) => HasBuilder (Vector n a) where
  type VecBuilder (Vector n a) = VectorBuilder n a
  sumBuilder = sumVectorBuilder
  identityBuilder x = (Just (Sum x), mempty)


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

type instance DualOf (Sparse v) = Sparse (DualOf v)
