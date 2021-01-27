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
    TensorProduct(..),
    LinearFunction,
    AFunction(..), transposeFunc,
    LinearFunction'(..)
)
where
import Data.Constraint
import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import GHC.Generics (Generic)

class TensorProduct u v w | u v -> w where
  (⊗) :: u -> v -> w

class TensorProduct u v (u⊗v) => TensorProduct' u v where
  type u ⊗ v :: Type

class TensorProduct dv v b => BVector b v dv where

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
  , TensorProduct f u v
  , TensorProduct dv f du
  ) => LinearFunction f u v du dv where

class
  ( 
  ) => LinearFunction' f where
    transpose :: LinearFunction (f u du v dv) u v du dv => f u du v dv -> f dv v du u
    transposeC :: LinearFunction (f u du v dv) u v du dv :- LinearFunction (f dv v du u) dv du v u

_testLinearFunction :: forall b f u v du dv. (Eq b, BVector b u du, BVector b v dv) => LinearFunction f u v du dv => f -> dv -> u -> Bool
_testLinearFunction f dv u = lhs == rhs
    where lhs = (dv ⊗ f :: du) ⊗ u :: b
          rhs = dv ⊗ (f ⊗ u) :: b

data AFunction u du v dv where
    IndentityFunc :: AFunction u du u du
    NegateFunc :: (AdditiveGroup u, AdditiveGroup du) => AFunction u du u du
    ScaleFunc :: forall a v dv. (VectorSpace v, VectorSpace dv, a ~ Scalar v, a ~ Scalar dv) => a -> AFunction v dv v dv
    BlackBoxFunc :: (u -> v) -> (dv -> du) -> AFunction u du v dv

instance TensorProduct (AFunction u du v dv) u v where
    IndentityFunc ⊗ x = x
    NegateFunc ⊗ x = negateV x
    ScaleFunc a ⊗ v = a *^ v
    (BlackBoxFunc f _) ⊗ x = f x

instance TensorProduct dv (AFunction u du v dv) du where
    x ⊗ IndentityFunc = x
    x ⊗ NegateFunc = negateV x
    v ⊗ ScaleFunc a = a *^ v
    x ⊗ (BlackBoxFunc _ f) = f x

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
