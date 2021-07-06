{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Grad
  ( HasGrad (..)
  )
where

-- TODO: remove constraint `DualOf (Needle p)

import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace (Scalar))
import Downhill.Linear.Expr (BasicVector)

class BasicVector (Grad p) => HasGrad p where
  type Grad p :: Type
  type Diff p :: Type
  evalGrad :: Grad p -> Diff p -> Scalar (Grad p)

instance
  ( AdditiveGroup s,
    Scalar (Grad a) ~ s,
    Scalar (Grad b) ~ s,
    HasGrad a,
    HasGrad b
  ) =>
  HasGrad (a, b)
  where
  type Grad (a, b) = (Grad a, Grad b)
  type Diff (a, b) = (Diff a, Diff b)
  evalGrad (a, b) (x, y) = evalGrad @a a x ^+^ evalGrad @b b y

instance
  ( AdditiveGroup s,
    Scalar (Grad a) ~ s,
    Scalar (Grad b) ~ s,
    Scalar (Grad c) ~ s,
    HasGrad a,
    HasGrad b,
    HasGrad c
  ) =>
  HasGrad (a, b, c)
  where
  type Grad (a, b, c) = (Grad a, Grad b, Grad c)
  type Diff (a, b, c) = (Diff a, Diff b, Diff c)
  evalGrad (a, b, c) (x, y, z) = evalGrad @a a x ^+^ evalGrad @b b y ^+^ evalGrad @c c z

class (Scalar (Grad p) ~ Scalar (Diff p), HasGrad p) => HasDiff p

instance HasGrad Float where
  type Grad Float = Float
  type Diff Float = Float
  evalGrad = (*)

instance HasDiff Float

instance HasGrad Double where
  type Grad Double = Double
  type Diff Double = Double
  evalGrad = (*)

instance HasDiff Double

instance
  ( HasDiff u,
    HasDiff v,
    da ~ Scalar (Diff u),
    da ~ Scalar (Diff v),
    a ~ Scalar u,
    a ~ Scalar v,
    AdditiveGroup (Diff a),
    AdditiveGroup da
  ) =>
  HasDiff (u, v)

instance
  ( HasDiff u,
    HasDiff v,
    HasDiff w,
    a ~ Scalar (Diff u),
    a ~ Scalar (Diff v),
    a ~ Scalar (Diff w),
    a ~ Scalar u,
    a ~ Scalar v,
    a ~ Scalar w,
    AdditiveGroup (Diff a),
    AdditiveGroup a
  ) =>
  HasDiff (u, v, w)
  where
