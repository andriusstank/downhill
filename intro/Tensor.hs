{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# language ScopedTypeVariables #-}
{-# language QuantifiedConstraints #-}
{-# language AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tensor (
    Vec(..),
    Bilinear(..), Bilinear'',
    AFunction(..), transposeFunc,
)
where
import Data.Constraint
import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import GHC.Generics (Generic)

newtype Vec dx = Vec { unVec :: dx }
    deriving Show
    deriving AdditiveGroup via dx

newtype Vec' dx x = Vec' { unVec' :: x }
    deriving Show
    deriving AdditiveGroup via x

newtype Covec' dx x = Covec' { uncovec' :: dx }
    deriving Show
    deriving AdditiveGroup via dx

-- instance AdditiveGroup *

type Bilinear'' u v w = (Bilinear u v, u ✕ v ~ w)

class Bilinear u v where
  type u ✕ v :: Type
  (✕) :: u -> v -> u ✕ v


data AFunction u du v dv where
    IndentityFunc :: AFunction u du u du
    NegateFunc :: (AdditiveGroup u, AdditiveGroup du) => AFunction u du u du
    ScaleFunc :: forall a v dv. (VectorSpace v, VectorSpace dv, a ~ Scalar v, a ~ Scalar dv) => a -> AFunction v dv v dv
    BlackBoxFunc :: (u -> v) -> (dv -> du) -> AFunction u du v dv

instance Bilinear (AFunction u du v dv) (Vec u) where
    type (AFunction u du v dv) ✕ (Vec u) = Vec v
    IndentityFunc ✕ x = x
    NegateFunc ✕ Vec x = Vec (negateV x)
    ScaleFunc a ✕ Vec v = Vec (a *^ v)
    (BlackBoxFunc f _) ✕ Vec x = Vec (f x)

instance Bilinear (Vec dv) (AFunction u du v dv) where
    type (Vec dv) ✕ (AFunction u du v dv) = Vec du
    x ✕ IndentityFunc = x
    Vec x ✕ NegateFunc = Vec (negateV x)
    Vec v ✕ ScaleFunc a = Vec (a *^ v)
    Vec x ✕ (BlackBoxFunc _ f) = Vec (f x)

transposeFunc :: AFunction u v du dv -> AFunction dv du v u
transposeFunc = \case
    IndentityFunc -> IndentityFunc
    NegateFunc -> NegateFunc
    ScaleFunc x -> ScaleFunc x
    BlackBoxFunc f g -> BlackBoxFunc g f
