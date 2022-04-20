{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Downhill.Grad
  ( Dual (..),
    HasGrad (..),
    GradBuilder,
    HasGradAffine,
  )
where

import Data.AffineSpace (AffineSpace (Diff))
import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace)
import qualified Data.VectorSpace as VectorSpace
import Downhill.Linear.Expr (BasicVector (VecBuilder))

-- | Dual of a vector @v@ is a linear map @v -> Scalar v@.
class
  ( AdditiveGroup s,
    VectorSpace v,
    VectorSpace dv,
    VectorSpace.Scalar v ~ s,
    VectorSpace.Scalar dv ~ s
  ) =>
  Dual s v dv
  where
  -- if evalGrad goes to HasGrad class, parameter p is ambiguous
  evalGrad :: dv -> v -> s



-- | @HasGrad@ is a collection of types and constraints that are useful
-- in many places. It helps to keep type signatures short.
class
  ( Dual (MScalar p) (Tang p) (Grad p),
    BasicVector (Tang p),
    BasicVector (Grad p)
  ) =>
  HasGrad p
  where
  -- | Scalar of @Tang p@ and @Grad p@.
  type MScalar p :: Type

  -- | Tangent vector of manifold @p@. If p is 'AffineSpace', @Tang p@ should
  -- be @'Diff' p@. If @p@ is 'VectorSpace', @Tang p@ might be the same as @p@ itself.
  type Tang p :: Type

  -- | Dual of tangent space of @p@.
  type Grad p :: Type

type GradBuilder v = VecBuilder (Grad v)

type HasGradAffine p =
  ( AffineSpace p,
    HasGrad p,
    HasGrad (Tang p),
    Tang p ~ Diff p,
    Tang (Tang p) ~ Tang p,
    Grad (Tang p) ~ Grad p
  )

instance Dual Integer Integer Integer where
  evalGrad = (*)

instance HasGrad Integer where
  type MScalar Integer = Integer
  type Tang Integer = Integer
  type Grad Integer = Integer

instance (Dual s a da, Dual s b db) => Dual s (a, b) (da, db) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s a da, Dual s b db, Dual s c dc) => Dual s (a, b, c) (da, db, dc) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance
  ( HasGrad a,
    HasGrad b,
    MScalar b ~ MScalar a
  ) =>
  HasGrad (a, b)
  where
  type MScalar (a, b) = MScalar a
  type Grad (a, b) = (Grad a, Grad b)
  type Tang (a, b) = (Tang a, Tang b)

instance
  ( HasGrad a,
    HasGrad b,
    HasGrad c,
    MScalar b ~ MScalar a,
    MScalar c ~ MScalar a
  ) =>
  HasGrad (a, b, c)
  where
  type MScalar (a, b, c) = MScalar a
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Tang (a, b, c) = (Tang a, Tang b, Tang c)

instance Dual Float Float Float where
  evalGrad = (*)

instance HasGrad Float where
  type MScalar Float = Float
  type Grad Float = Float
  type Tang Float = Float

instance Dual Double Double Double where
  evalGrad = (*)

instance HasGrad Double where
  type MScalar Double = Double
  type Grad Double = Double
  type Tang Double = Double
