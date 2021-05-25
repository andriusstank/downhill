{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Downhill.Linear.Expr
(
    -- * Expression
    Expr(..),
    Term(..),
    -- * Vectors
    BasicVector(..), FullVector(..),
    SparseVector(..),
    -- * Functions
    FwdFun(..), BackFun(..), flipFwdFun, flipBackFun,
    -- * Misc
    zeroExpr, sumExpr,
    maybeToMonoid
)
where

import Data.Kind (Type)
import Data.Semigroup (Sum (Sum, getSum))
import Data.Maybe (fromMaybe)
import Data.VectorSpace (VectorSpace(..), AdditiveGroup(..))

-- |Linear function 'e u v' applied to single argument of type 'u'.
data Term e a v where
    Term :: e u v -> Expr e a u -> Term e a v

-- |@Expr e a v@ represents a linear expression of type @v@, containing some free variables of type @a@.
-- @e@ is a type of the edge in computational graph, 'BackFun' for reverse mode AD.
data Expr e a v where
    ExprVar :: Expr e a a
    ExprSum :: BasicVector v => [Term e a v] -> Expr e a v

-- |@VecBuilder v@ is a sparse representation of vector @v@. Edges of a computational graph
-- produce builders, which are then summed into vectors in nodes.
class Monoid (VecBuilder v) => BasicVector v where
    type VecBuilder v :: Type
    sumBuilder :: VecBuilder v -> v

maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

instance (BasicVector a, BasicVector b) => BasicVector (a, b) where
    type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
    sumBuilder = sumPair . maybeToMonoid
        where sumPair (a, b) = (sumBuilder a, sumBuilder b)

instance BasicVector Float where
    type VecBuilder Float = Sum Float
    sumBuilder = getSum

instance BasicVector Double where
    type VecBuilder Double = Sum Double
    sumBuilder = getSum

-- |Full-featured vector. Linear function form a vector space, @FullVector@ class provides all the functionality
-- to implement 'VectorSpace' instance 
-- for 'Expr'.
class BasicVector v => FullVector v where
    identityBuilder :: v -> VecBuilder v
    negateBuilder :: v -> VecBuilder v
    scaleBuilder :: Scalar v -> v -> VecBuilder v

instance FullVector Float where
    identityBuilder = Sum
    negateBuilder = Sum . negate
    scaleBuilder x = Sum . (x*)
instance FullVector Double where
    identityBuilder = Sum
    negateBuilder = Sum . negate
    scaleBuilder x = Sum . (x*)
instance (Scalar a ~ Scalar b, FullVector a, FullVector b) => FullVector (a, b) where
    identityBuilder (x, y) = Just (identityBuilder x, identityBuilder y)
    negateBuilder (x, y) = Just (negateBuilder x, negateBuilder y)
    scaleBuilder a (x, y) = Just (scaleBuilder a x, scaleBuilder a y)

-- | Sometimes we need to forward gradients through the node without summing them. 
-- The type of the node would be @VecBuilder v@, but what would be @VecBuilder (VecBuilder v)@?
newtype SparseVector v = SparseVector { unSparseVector :: VecBuilder v }

deriving via (VecBuilder v) instance Semigroup (VecBuilder v) => Semigroup (SparseVector v)

instance Monoid (VecBuilder v) => BasicVector (SparseVector v) where
    type VecBuilder (SparseVector v) = VecBuilder v
    sumBuilder = SparseVector


-- | Edge type for backward mode evaluation
newtype BackFun u v = BackFun { unBackFun :: v -> VecBuilder u }

-- | Edge type for forward mode evaluation
newtype FwdFun u v = FwdFun  {unFwdFun :: u -> VecBuilder v }

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
    where wrap x = Term (BackFun identityBuilder) x

