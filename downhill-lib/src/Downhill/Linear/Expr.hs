{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Linear.Expr
  ( -- * Expression
    Expr (..),
    Term (..),

    -- * Vectors
    BasicVector (..),
    SparseVector (..),
    DenseVector (..),
    DenseBuilder (..),
    toDenseBuilder,

    -- * Generics
    genericSumBuilder,
    genericIdentityBuilder,
    genericSumMaybeBuilder,
    genericIdentityMaybeBuilder,

    -- * Misc
    maybeToMonoid,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (Sum, getSum))
import Data.VectorSpace (AdditiveGroup (..), VectorSpace (..), zeroV)
import GHC.Generics (Generic (Rep, from, to), K1 (K1), M1 (M1), U1 (U1), V1, (:*:) ((:*:)))

-- | Argument @f@ in @Term f x@ must be /linear/ function. That's a law.
data Term a v where
  Term :: (v -> VecBuilder u) -> Expr a u -> Term a v

-- | @Expr a v@ represents a linear expression of type @v@, containing some free variables of type @a@.
data Expr a v where
  ExprVar :: Expr a a
  ExprSum :: BasicVector v => [Term a v] -> Expr a v

class Monoid (VecBuilder v) => BasicVector v where
  -- | @VecBuilder v@ is a sparse representation of vector @v@. Edges of a computational graph
  -- produce builders, which are then summed into vectors in nodes. Monoid operation '<>'
  -- means addition of vectors, but it doesn't need to compute the sum immediately - it
  -- might defer computation until 'sumBuilder' is evaluated.
  --
  -- @
  -- sumBuilder mempty = zeroV
  -- sumBuilder (x <> y) = sumBuilder x ^+^ sumBuilder y
  -- @
  --
  -- 'mempty' must be cheap. '<>' must be O(1).
  type VecBuilder v :: Type

  sumBuilder :: VecBuilder v -> v
  identityBuilder :: v -> VecBuilder v

  default sumBuilder ::
    forall b.
    ( VecBuilder v ~ Maybe b,
      Generic b,
      Generic v,
      GBasicVector (Rep b) (Rep v),
      AdditiveGroup v
    ) =>
    VecBuilder v ->
    v
  sumBuilder = genericSumMaybeBuilder @b @v

  default identityBuilder ::
    forall b.
    ( VecBuilder v ~ Maybe b,
      Generic b,
      Generic v,
      GBasicVector (Rep b) (Rep v),
      AdditiveGroup v
    ) =>
    v ->
    VecBuilder v
  identityBuilder = genericIdentityMaybeBuilder @b @v

maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

_maybeToVector :: AdditiveGroup v => Maybe v -> v
_maybeToVector = fromMaybe zeroV

instance BasicVector Integer where
  type VecBuilder Integer = Sum Integer
  sumBuilder = getSum
  identityBuilder = Sum

instance (BasicVector a, BasicVector b) => BasicVector (a, b) where
  type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
  sumBuilder = sumPair . maybeToMonoid
    where
      sumPair (a, b) = (sumBuilder a, sumBuilder b)
  identityBuilder (x, y) = Just (identityBuilder x, identityBuilder y)

instance (BasicVector a, BasicVector b, BasicVector c) => BasicVector (a, b, c) where
  type VecBuilder (a, b, c) = Maybe (VecBuilder a, VecBuilder b, VecBuilder c)
  sumBuilder = sumTriple . maybeToMonoid
    where
      sumTriple (a, b, c) = (sumBuilder a, sumBuilder b, sumBuilder c)
  identityBuilder (x, y, z) = Just (identityBuilder x, identityBuilder y, identityBuilder z)

instance BasicVector Float where
  type VecBuilder Float = Sum Float
  sumBuilder = getSum
  identityBuilder = Sum

instance BasicVector Double where
  type VecBuilder Double = Sum Double
  sumBuilder = getSum
  identityBuilder = Sum

-- |  Normally graph node would compute the sum of gradients and then
-- propagate it to ancestor nodes. That's the best strategy when
-- some computation needs to be performed for backpropagation.
-- Some operations, like constructing/deconstructing tuples or
-- wrapping/unwrapping, don't need to compute the sum. Doing so only
-- destroys sparsity. A node of type @SparseVector v@ won't sum
-- the gradients, it will simply forward builders to its parents.
newtype SparseVector v = SparseVector
  {unSparseVector :: VecBuilder v}

deriving via (VecBuilder v) instance Semigroup (VecBuilder v) => Semigroup (SparseVector v)

instance Monoid (VecBuilder v) => BasicVector (SparseVector v) where
  type VecBuilder (SparseVector v) = VecBuilder v
  sumBuilder = SparseVector
  identityBuilder = unSparseVector

newtype DenseSemibuilder v = DenseSemibuilder {_unDenseSemibuilder :: v}

instance AdditiveGroup v => Semigroup (DenseSemibuilder v) where
  DenseSemibuilder x <> DenseSemibuilder y = DenseSemibuilder (x ^+^ y)

newtype DenseBuilder v = DenseBuilder (Maybe v)
  deriving (Semigroup, Monoid) via (Maybe (DenseSemibuilder v))

toDenseBuilder :: v -> DenseBuilder v
toDenseBuilder = DenseBuilder . Just

-- | When sparsity is not needed, we can use vector @v@ as a builder of itself.
-- @DenseVector@ takes care of that.
newtype DenseVector v = DenseVector v
  deriving (AdditiveGroup, VectorSpace) via v

instance AdditiveGroup v => BasicVector (DenseVector v) where
  type VecBuilder (DenseVector v) = DenseBuilder v
  sumBuilder (DenseBuilder Nothing) = DenseVector zeroV
  sumBuilder (DenseBuilder (Just x)) = DenseVector x
  identityBuilder (DenseVector v) = DenseBuilder (Just v)

class GBasicVector b v where
  gsumBuilder :: b p -> v p
  gidentityBuilder :: v p -> b p

instance (BasicVector v, b ~ VecBuilder v) => GBasicVector (K1 x b) (K1 x v) where
  gsumBuilder (K1 x) = K1 (sumBuilder x)
  gidentityBuilder (K1 x) = K1 (identityBuilder x)

instance (GBasicVector b v) => GBasicVector (M1 x y b) (M1 x y' v) where
  gsumBuilder (M1 x) = M1 (gsumBuilder x)
  gidentityBuilder (M1 x) = M1 (gidentityBuilder x)

instance (GBasicVector bu u, GBasicVector bv v) => GBasicVector (bu :*: bv) (u :*: v) where
  gsumBuilder (x1 :*: x2) = gsumBuilder x1 :*: gsumBuilder x2
  gidentityBuilder (x1 :*: x2) = gidentityBuilder x1 :*: gidentityBuilder x2

instance GBasicVector V1 V1 where
  gsumBuilder = \case {}
  gidentityBuilder = \case {}

instance GBasicVector U1 U1 where
  gsumBuilder U1 = U1
  gidentityBuilder U1 = U1

genericSumBuilder :: forall b v. (Generic b, Generic v, GBasicVector (Rep b) (Rep v)) => b -> v
genericSumBuilder = to . gsumBuilder . from

genericIdentityBuilder :: forall b v. (Generic b, Generic v, GBasicVector (Rep b) (Rep v)) => v -> b
genericIdentityBuilder = to . gidentityBuilder . from

genericSumMaybeBuilder ::
  forall b v.
  (Generic b, Generic v, AdditiveGroup v, GBasicVector (Rep b) (Rep v)) =>
  Maybe b ->
  v
genericSumMaybeBuilder = maybe zeroV genericSumBuilder

genericIdentityMaybeBuilder :: forall b v. (Generic b, Generic v, GBasicVector (Rep b) (Rep v)) => v -> Maybe b
genericIdentityMaybeBuilder = Just . genericIdentityBuilder
