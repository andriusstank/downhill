{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.V2 (V2(..), M2(..))
where

import GHC.Generics (Generically)
import NumHask.Prelude
import Downhill.Orphans
import DownhillTest.Transpose (Transpose(..))
import Downhill.Linear.Fun (DualOf, LinearMap)
import Downhill.VSpace (VSpace(..), RealScalar, InnerSpace (..))
import Downhill.VSpace (CScalar(..), InnerSpace)

data V2 a = V2 a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (V2 a))
   
type instance DualOf (V2 a) = Transpose (V2 a)

instance CScalar a => VSpace (V2 a) where
    Transpose (V2 dx dy) |*| V2 x y = dx |*| x + dy |*| y

instance CScalar a => InnerSpace (V2 a) where
  riesz (V2 x y) = Transpose (V2 (conj x) (conj y))
  coriesz (Transpose (V2 x y)) = V2 (conj x) (conj y)
  V2 x1 y1 <.> V2 x2 y2 = x1 <.> x2 + y1 <.> y2


data M2 a = M2 a a a a
    deriving Generic
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (M2 a))

