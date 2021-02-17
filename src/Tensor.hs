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
    TensorProduct(..), TensorProduct'',
    LinearFunction,
    AFunction(..), transposeFunc,
    LinearFunction'(..)
)
where
import Data.Constraint
import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import GHC.Generics (Generic)

newtype Vec dx = Vec { unVec :: dx }
    deriving Show
    deriving AdditiveGroup via dx

-- instance AdditiveGroup *

type TensorProduct'' u v w = (TensorProduct u v, u ⊗ v ~ w)

class TensorProduct u v where
  type u ⊗ v :: Type
  (⊗) :: u -> v -> u ⊗ v

class TensorProduct'' (Vec dv) (Vec v) (Vec b) => BVector b v dv where

-- f: u -> v
-- dv ~ Dual b v
-- du ~ Dual b u
--  f ⊗ u ::  v
-- dv ⊗ f :: du
-- (dv ⊗ f) ⊗ u == dv ⊗ (f ⊗ u)
class
  ( AdditiveGroup u
  , AdditiveGroup v
  , AdditiveGroup du
  , AdditiveGroup dv
  , TensorProduct'' f (Vec u) (Vec v)
  , TensorProduct'' (Vec dv) f (Vec du)
  ) => LinearFunction f u v du dv where

class
  ( 
  ) => LinearFunction' f where
    transpose :: LinearFunction (f u du v dv) u v du dv => f u du v dv -> f dv v du u
    transposeC :: LinearFunction (f u du v dv) u v du dv :- LinearFunction (f dv v du u) dv du v u

_testLinearFunction :: forall b f u v du dv. (Eq b, BVector b u du, BVector b v dv) => LinearFunction f u v du dv => f -> Vec dv -> Vec u -> Bool
_testLinearFunction f dv u = lhs == rhs
    where Vec lhs = (dv ⊗ f :: Vec du) ⊗ u :: Vec b
          Vec rhs = dv ⊗ (f ⊗ u) :: Vec b

data AFunction u du v dv where
    IndentityFunc :: AFunction u du u du
    NegateFunc :: (AdditiveGroup u, AdditiveGroup du) => AFunction u du u du
    ScaleFunc :: forall a v dv. (VectorSpace v, VectorSpace dv, a ~ Scalar v, a ~ Scalar dv) => a -> AFunction v dv v dv
    BlackBoxFunc :: (u -> v) -> (dv -> du) -> AFunction u du v dv

instance TensorProduct (AFunction u du v dv) (Vec u) where
    type (AFunction u du v dv) ⊗ (Vec u) = Vec v
    IndentityFunc ⊗ x = x
    NegateFunc ⊗ Vec x = Vec (negateV x)
    ScaleFunc a ⊗ Vec v = Vec (a *^ v)
    (BlackBoxFunc f _) ⊗ Vec x = Vec (f x)

instance TensorProduct (Vec dv) (AFunction u du v dv) where
    type (Vec dv) ⊗ (AFunction u du v dv) = Vec du
    x ⊗ IndentityFunc = x
    Vec x ⊗ NegateFunc = Vec (negateV x)
    Vec v ⊗ ScaleFunc a = Vec (a *^ v)
    Vec x ⊗ (BlackBoxFunc _ f) = Vec (f x)

transposeFunc :: AFunction u v du dv -> AFunction dv du v u
transposeFunc = \case
    IndentityFunc -> IndentityFunc
    NegateFunc -> NegateFunc
    ScaleFunc x -> ScaleFunc x
    BlackBoxFunc f g -> BlackBoxFunc g f

instance (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => LinearFunction (AFunction u du v dv) u v du dv

instance LinearFunction' AFunction where
    transpose = transposeFunc
    transposeC = Sub Dict
