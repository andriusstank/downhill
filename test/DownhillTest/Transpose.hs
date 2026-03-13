{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.Transpose (Transpose (..)) where

import Downhill.Linear.Fun (DualOf, LinearMap (..))
import Downhill.VSpace (InnerSpace (..))
import NumHask.Prelude

newtype Transpose a = Transpose {transpose :: a}
    deriving (Generic, Show, Eq)
    deriving (Additive, Subtractive, Multiplicative, MultiplicativeAction) via a

type instance DualOf (Transpose a) = a

instance
    ( LinearMap f u v
    , DualOf u ~ Transpose u
    , DualOf v ~ Transpose v
    , DualOf (Scalar v) ~ Scalar v
    , Scalar (Scalar v) ~ Scalar v
    , InnerSpace v
    ) =>
    LinearMap (Transpose f) v u
    where
    f <*| x = transpose (Transpose x |*> transpose f)
    x |*> f = Transpose $ transpose f <*| transpose x
