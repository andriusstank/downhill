{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Downhill.Grad
  ( Dual(..), HasGrad (..)
  )
where

import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)))
import Downhill.Linear.Expr (BasicVector)

class AdditiveGroup s => Dual s dv v where
  evalGrad :: dv -> v -> s

class (Dual s (Grad p) (Diff p), BasicVector (Grad p)) => HasGrad s p | p -> s where
  type Diff p :: Type
  type Grad p :: Type

instance (Dual s da a, Dual s db b) => Dual s (da, db) (a, b) where
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance (Dual s da a, Dual s db b, Dual s dc c) => Dual s (da, db, dc) (a, b, c) where
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

instance
  ( HasGrad s a,
    HasGrad s b
  ) =>
  HasGrad s (a, b)
  where
  type Grad (a, b) = (Grad a, Grad b)
  type Diff (a, b) = (Diff a, Diff b)

instance
  ( HasGrad s a,
    HasGrad s b,
    HasGrad s c
  ) =>
  HasGrad s (a, b, c)
  where
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Diff (a, b, c) = (Diff a, Diff b, Diff c)
  
instance Dual Float Float Float where
  evalGrad = (*)

instance HasGrad Float Float where
  type Grad Float = Float
  type Diff Float = Float

instance Dual Double Double Double where
  evalGrad = (*)
instance HasGrad Double Double where
  type Grad Double = Double
  type Diff Double = Double
