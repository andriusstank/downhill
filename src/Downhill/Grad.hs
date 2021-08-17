{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Downhill.Grad
  ( Dual (..),
    HasGrad (..), GradBuilder, HasFullGrad, HasGradAffine
  )
where

import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace)
import Downhill.Linear.Expr (FullVector, BasicVector (VecBuilder))
import Data.AffineSpace (AffineSpace)

import qualified Data.VectorSpace as VectorSpace
import qualified Data.AffineSpace as AffineSpace


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

class
  ( Dual (Scalar p) (Tang p) (Grad p)
  , BasicVector (Grad p)
  ) =>
  HasGrad p
  where
  type Scalar p :: Type
  type Tang p :: Type
  type Grad p :: Type

type GradBuilder v = VecBuilder (Grad v)

type HasFullGrad p = (HasGrad p, FullVector (Grad p))

type HasGradAffine p =
  ( AffineSpace p,
    HasGrad p,
    HasGrad (Tang p),
    Tang p ~ AffineSpace.Diff p,
    Grad (Tang p) ~ Grad p
  )

instance Dual Integer Integer Integer  where
  evalGrad = (*)

instance HasGrad Integer where
  type Scalar Integer = Integer
  type Tang Integer = Integer
  type Grad Integer = Integer

instance (Dual s a da, Dual s b db) => Dual s (a, b) (da, db) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s a da, Dual s b db, Dual s c dc) => Dual s (a, b, c) (da, db, dc) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance
  ( HasGrad a,
    HasGrad b,
    Scalar b ~ Scalar a
  ) =>
  HasGrad (a, b)
  where
  type Scalar (a, b) = Scalar a
  type Grad (a, b) = (Grad a, Grad b)
  type Tang (a, b) = (Tang a, Tang b)

instance
  ( HasGrad a,
    HasGrad b,
    HasGrad c,
    Scalar b ~ Scalar a,
    Scalar c ~ Scalar a
  ) =>
  HasGrad (a, b, c)
  where
  type Scalar (a, b, c) = Scalar a
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Tang (a, b, c) = (Tang a, Tang b, Tang c)

instance Dual Float Float Float where
  evalGrad = (*)

instance HasGrad Float where
  type Scalar Float = Float
  type Grad Float = Float
  type Tang Float = Float

instance Dual Double Double Double where
  evalGrad = (*)

instance HasGrad Double where
  type Scalar Double = Double
  type Grad Double = Double
  type Tang Double = Double
