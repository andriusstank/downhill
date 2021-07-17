{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Downhill.Grad
  ( Dual (..),
    HasGrad (..),
  )
where

import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace)
import Downhill.Linear.Expr (FullVector)

import qualified Data.VectorSpace as VectorSpace

class
  ( AdditiveGroup s,
    VectorSpace v,
    VectorSpace dv,
    VectorSpace.Scalar v ~ s,
    VectorSpace.Scalar dv ~ s
  ) =>
  Dual s dv v
  where
  evalGrad :: dv -> v -> s

class (Dual (Scalar p) (Grad p) (Diff p), FullVector (Grad p)) => HasGrad p where
  type Scalar p :: Type
  type Diff p :: Type
  type Grad p :: Type

instance (Dual s da a, Dual s db b) => Dual s (da, db) (a, b) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s da a, Dual s db b, Dual s dc c) => Dual s (da, db, dc) (a, b, c) where
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
  type Diff (a, b) = (Diff a, Diff b)

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
  type Diff (a, b, c) = (Diff a, Diff b, Diff c)

instance Dual Float Float Float where
  evalGrad = (*)

instance HasGrad Float where
  type Scalar Float = Float
  type Grad Float = Float
  type Diff Float = Float

instance Dual Double Double Double where
  evalGrad = (*)

instance HasGrad Double where
  type Scalar Double = Double
  type Grad Double = Double
  type Diff Double = Double
