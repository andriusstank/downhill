{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Linear.Expr
  ( -- * Expression
    Expr (..),
    Term (..),

    -- * Vectors
    BasicVector (..),
    FullVector (..),
    SparseVector (..),
    DenseVector (..),
    DenseBuilder (..),
    toDenseBuilder,

    -- * Functions
    FwdFun (..),
    BackFun (..),
    flipFwdFun,
    flipBackFun,

    -- * Misc
    zeroExpr,
    sumExpr,
    maybeToMonoid,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (Sum, getSum))
import Data.VectorSpace (AdditiveGroup (..), VectorSpace (..))

-- | Linear function 'e u v' applied to single argument of type 'u'.
data Term e a v where
  Term :: e u v -> Expr e a u -> Term e a v

-- | @Expr e a v@ represents a linear expression of type @v@, containing some free variables of type @a@.
--  @e@ is a type of the edge in computational graph, 'BackFun' for reverse mode AD.
data Expr e a v where
  ExprVar :: Expr e a a
  ExprSum :: BasicVector v => [Term e a v] -> Expr e a v

class Monoid (VecBuilder v) => BasicVector v where
  {- | @VecBuilder v@ is a sparse representation of vector @v@. Edges of a computational graph
    produce builders, which are then summed into vectors in nodes. Monoid operation '<>'
    means addition of vectors, but it doesn't need to compute the sum immediately - it
    might defer computation until 'sumBuilder' is evaluated.

@
sumBuilder mempty = zeroV
sumBuilder (x <> y) = sumBuilder x ^+^ sumBuilder y
@

    'mempty' must be cheap. '<>' must be O(1).
-}
  type VecBuilder v :: Type

  sumBuilder :: VecBuilder v -> v

maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

instance BasicVector Integer where
  type VecBuilder Integer = Sum Integer
  sumBuilder = getSum

instance (BasicVector a, BasicVector b) => BasicVector (a, b) where
  type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
  sumBuilder = sumPair . maybeToMonoid
    where
      sumPair (a, b) = (sumBuilder a, sumBuilder b)

instance (BasicVector a, BasicVector b, BasicVector c) => BasicVector (a, b, c) where
  type VecBuilder (a, b, c) = Maybe (VecBuilder a, VecBuilder b, VecBuilder c)
  sumBuilder = sumTriple . maybeToMonoid
    where
      sumTriple (a, b, c) = (sumBuilder a, sumBuilder b, sumBuilder c)

instance BasicVector Float where
  type VecBuilder Float = Sum Float
  sumBuilder = getSum

instance BasicVector Double where
  type VecBuilder Double = Sum Double
  sumBuilder = getSum

-- | Full-featured vector.
--
-- Gradients are linear functions and form a vector space.
-- @FullVector@ class provides functionality that is needed to
-- make 'VectorSpace' instances.
class (BasicVector v, VectorSpace v) => FullVector v where
  identityBuilder :: v -> VecBuilder v
  negateBuilder :: v -> VecBuilder v
  scaleBuilder :: Scalar v -> v -> VecBuilder v

instance FullVector Float where
  identityBuilder = Sum
  negateBuilder = Sum . negate
  scaleBuilder x = Sum . (x *)

instance FullVector Double where
  identityBuilder = Sum
  negateBuilder = Sum . negate
  scaleBuilder x = Sum . (x *)

instance FullVector Integer where
  identityBuilder = Sum
  negateBuilder = Sum . negate
  scaleBuilder x = Sum . (x *)

instance (Scalar a ~ Scalar b, FullVector a, FullVector b) => FullVector (a, b) where
  identityBuilder (x, y) = Just (identityBuilder x, identityBuilder y)
  negateBuilder (x, y) = Just (negateBuilder x, negateBuilder y)
  scaleBuilder a (x, y) = Just (scaleBuilder a x, scaleBuilder a y)

instance (s ~ Scalar a, s ~ Scalar b, s ~ Scalar c, FullVector a, FullVector b, FullVector c) => FullVector (a, b, c) where
  identityBuilder (x, y, z) = Just (identityBuilder x, identityBuilder y, identityBuilder z)
  negateBuilder (x, y, z) = Just (negateBuilder x, negateBuilder y, negateBuilder z)
  scaleBuilder a (x, y, z) = Just (scaleBuilder a x, scaleBuilder a y, scaleBuilder a z)

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

instance VectorSpace v => FullVector (DenseVector v) where
  identityBuilder (DenseVector v) = DenseBuilder (Just v)
  negateBuilder (DenseVector v) = DenseBuilder (Just (negateV v))
  scaleBuilder a (DenseVector v) = DenseBuilder (Just (a *^ v))

-- | Edge type for backward mode evaluation
newtype BackFun u v = BackFun {unBackFun :: v -> VecBuilder u}

-- | Edge type for forward mode evaluation
newtype FwdFun u v = FwdFun {unFwdFun :: u -> VecBuilder v}

flipBackFun :: BackFun u v -> FwdFun v u
flipBackFun (BackFun f) = FwdFun f

flipFwdFun :: FwdFun u v -> BackFun v u
flipFwdFun (FwdFun f) = BackFun f

instance FullVector v => AdditiveGroup (Expr BackFun a v) where
  zeroV = zeroExpr
  negateV x = ExprSum [Term (BackFun negateBuilder) x]
  x ^+^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun identityBuilder) y]
  x ^-^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun negateBuilder) y]

instance FullVector dv => VectorSpace (Expr BackFun da dv) where
  type Scalar (Expr BackFun da dv) = Scalar dv
  a *^ v = ExprSum [Term (BackFun (scaleBuilder a)) v]

zeroExpr :: BasicVector dv => Expr e da dv
zeroExpr = ExprSum []

sumExpr :: FullVector dv => [Expr BackFun da dv] -> Expr BackFun da dv
sumExpr xs = ExprSum (wrap <$> xs)
  where
    wrap x = Term (BackFun identityBuilder) x
