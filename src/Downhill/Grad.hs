{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Downhill.Grad (
    HasGrad(..),
    HasDiff(..)
)
where

-- TODO: remove constraint `DualOf (Needle p)
import Downhill.Linear.Expr (BasicVector)
import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace (Scalar))
import Data.Kind (Type)

class BasicVector (Grad p) => HasGrad p where
  type Grad p :: Type


instance
  ( AdditiveGroup s,
    Scalar (Grad a) ~ s,
    Scalar (Grad b) ~ s,
    HasGrad a,
    HasGrad b
  ) =>
  HasGrad (a, b) where
    type Grad (a, b) = (Grad a, Grad b)

instance
  ( AdditiveGroup s,
    Scalar (Grad a) ~ s,
    Scalar (Grad b) ~ s,
    Scalar (Grad c) ~ s,
    HasGrad a,
    HasGrad b,
    HasGrad c
  ) =>
  HasGrad (a, b, c) where
    type Grad (a, b, c) = (Grad a, Grad b, Grad c)

class (Scalar (Grad p) ~ Scalar (Diff p), HasGrad p) => HasDiff p
  where
  type Diff p :: Type
  evalGrad :: Grad p -> Diff p -> Scalar (Grad p)

instance HasGrad Float where
  type Grad Float = Float

instance HasDiff Float where
  type Diff Float = Float
  evalGrad = (*)

instance HasGrad Double where
  type Grad Double = Double

instance HasDiff Double where
  type Diff Double = Double
  evalGrad = (*)

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
  where
  type Diff (u, v) = (Diff u, Diff v)
  evalGrad (a, b) (x, y) = evalGrad @u a x ^+^ evalGrad @v b y

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
  type Diff (u, v, w) = (Diff u, Diff v, Diff w)
  evalGrad (a, b, c) (x, y, z) = evalGrad @u a x ^+^ evalGrad @v b y ^+^ evalGrad @w c z
