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

module Expr
(
    Expr(..), zeroE,
    Term(..),
    BasicVector(..), FullVector(..),
    SparseVector(..),
    maybeToMonoid
)
where

import Data.Kind (Type)
import Data.Semigroup (Sum (Sum, getSum))
import Data.Maybe (fromMaybe)
import Data.VectorSpace (VectorSpace(Scalar))
data Term e a v where
    Term :: e u v -> Expr e a u -> Term e a v

data Expr e a v where
    ExprVar :: Expr e a a
    ExprSum :: BasicVector v => [Term e a v] -> Expr e a v



class Monoid (VecBuilder v) => BasicVector v where
    type VecBuilder v :: Type
    sumBuilder :: VecBuilder v -> v

maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

sumPair :: (BasicVector a, BasicVector b) => (VecBuilder a, VecBuilder b) -> (a, b)
sumPair (a, b) = (sumBuilder a, sumBuilder b)

instance (BasicVector a, BasicVector b) => BasicVector (a, b) where
    type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
    sumBuilder = sumPair . maybeToMonoid

instance BasicVector Float where
    type VecBuilder Float = Sum Float
    sumBuilder = getSum

instance BasicVector Double where
    type VecBuilder Double = Sum Double
    sumBuilder = getSum

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

newtype SparseVector v = SparseVector { unSparseVector :: VecBuilder v }

deriving via (VecBuilder v) instance Semigroup (VecBuilder v) => Semigroup (SparseVector v)

instance Monoid (VecBuilder v) => BasicVector (SparseVector v) where
    type VecBuilder (SparseVector v) = VecBuilder v
    sumBuilder = SparseVector


zeroE :: BasicVector dv => Expr e da dv
zeroE = ExprSum []

