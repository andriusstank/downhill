{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Downhill.Grad
  ( Dual(..), HasGrad (..)
  )
where

-- TODO: remove constraint `DualOf (Needle p)

import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)))
import Downhill.Linear.Expr (BasicVector)

import qualified Data.VectorSpace as VectorSpace

class AdditiveGroup s => Dual s dv v where
  evalGrad :: dv -> v -> s

class (Dual (Scalar p) (Grad p) (Diff p), BasicVector (Grad p)) => HasGrad p where
  type Scalar p
  type Scalar p = VectorSpace.Scalar (Diff p)
  type Diff p :: Type
  type Grad p :: Type

instance (Dual s da a, Dual s db b) => Dual s (da, db) (a, b) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s da a, Dual s db b, Dual s dc c) => Dual s (da, db, dc) (a, b, c) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance
  ( AdditiveGroup s,
    Scalar a ~ s,
    Scalar b ~ s,
    HasGrad a,
    HasGrad b,
    Dual s (Grad a) (Diff a),
    Dual s (Grad b) (Diff b),
    VectorSpace.Scalar (Diff a) ~ s
  ) =>
  HasGrad (a, b)
  where
  type Scalar (a, b) = Scalar a
  type Grad (a, b) = (Grad a, Grad b)
  type Diff (a, b) = (Diff a, Diff b)

instance
  ( AdditiveGroup s,
    Scalar a ~ s,
    Scalar b ~ s,
    Scalar c ~ s,
    HasGrad a,
    HasGrad b,
    HasGrad c,
    Dual s (Grad a) (Diff a),
    Dual s (Grad b) (Diff b),
    Dual s (Grad c) (Diff c)
  ) =>
  HasGrad (a, b, c)
  where
  type Scalar (a, b, c) = Scalar a
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Diff (a, b, c) = (Diff a, Diff b, Diff c)
  
instance Dual Float Float Float where
  evalGrad = (*)

instance HasGrad Float where
  type Grad Float = Float
  type Diff Float = Float

instance Dual Double Double Double where
  evalGrad = (*)
instance HasGrad Double where
  type Grad Double = Double
  type Diff Double = Double
