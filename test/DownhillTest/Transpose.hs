{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
module DownhillTest.Transpose(Transpose(..)) where
import Downhill.Linear.Fun (LinearMap(..), DualOf)
import NumHask.Prelude

newtype Transpose a = Transpose { transpose :: a }
    deriving (Generic, Show, Eq)
    deriving (Additive, Subtractive, MultiplicativeAction) via a

type instance DualOf (Transpose a) = a

instance (LinearMap f u v, DualOf u ~ Transpose u, DualOf v ~ Transpose v) => LinearMap (Transpose f) (Transpose v) (Transpose u) where
    Transpose f <*| Transpose x = Transpose x |*> f
    x |*> Transpose f =  f <*| x

