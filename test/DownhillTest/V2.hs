{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.V2 (V2 (..), Cov2(..), M2 (..))
where

import Downhill.Linear.Fun (DualOf, LinearMap (..))
import Downhill.VSpace (CScalar (..), InnerSpace (..), VSpace (..))
import NumHask.Prelude

data V2 a = V2 a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (V2 a))

data Cov2 a = Cov2 a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (Cov2 a))

type instance DualOf (V2 a) = Cov2 a
type instance DualOf (Cov2 a) = V2 a

instance (CScalar a) => VSpace (V2 a) where
    Cov2 dx dy |*| V2 x y = dx |*| x + dy |*| y

instance (CScalar a) => InnerSpace (V2 a) where
    riesz (V2 x y) = Cov2 (conj x) (conj y)
    coriesz (Cov2 x y) = V2 (conj x) (conj y)
    V2 x1 y1 <.> V2 x2 y2 = x1 <.> x2 + y1 <.> y2

data M2 a = M2 a a a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (M2 a))

instance (Multiplicative a, Additive a) => Multiplicative (M2 a) where
    one = M2 one zero zero one
    M2 a00 a01 a10 a11 * M2 b00 b01 b10 b11 =
        M2
            (a00 * b00 + a01 * b10)
            (a00 * b01 + a01 * b11)
            (a10 * b00 + a11 * b10)
            (a10 * b01 + a11 * b11)

instance (CScalar a) => LinearMap (M2 a) (V2 a) (V2 a) where
    lmapFwd (M2 m00 m01 m10 m11) (V2 x y) = V2 x' y'
      where
        x' = m00 * x + m01 * y
        y' = m10 * x + m11 * y
    lmapBack (M2 m00 m01 m10 m11) (Cov2 x y) = Cov2 x' y'
      where
        x' = m00 * x + m10 * y
        y' = m01 * x + m11 * y

type instance DualOf (M2 a) = M2 a

instance (CScalar a) => VSpace (M2 a) where
    M2 d00 d01 d10 d11 |*| M2 m00 m01 m10 m11 = d00 * m00 + d10 * m01 + d01 * m10 + d11 * m11

instance (CScalar a) => InnerSpace (M2 a) where
    riesz (M2 m00 m01 m10 m11) = M2 (conj m00) (conj m10) (conj m01) (conj m11)
    coriesz (M2 m00 m01 m10 m11) = M2 (conj m00) (conj m10) (conj m01) (conj m11)
    M2 m00 m01 m10 m11 <.> M2 d00 d01 d10 d11 = m00 <.> d00 + m01 <.> d01 + m10 <.> d10 + m11 <.> d11
