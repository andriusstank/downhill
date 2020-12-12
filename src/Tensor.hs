{-# LANGUAGE RankNTypes #-}
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
    PairFunc(..),
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
  ) => LinearFunction b f u v du dv | f -> b where

class
  ( 
  ) => LinearFunction' b f  where
    transpose :: LinearFunction b (f u du v dv) u v du dv => f u du v dv -> f dv v du u
    transposeC :: LinearFunction b (f u du v dv) u v du dv :- LinearFunction b (f dv v du u) dv du v u

_testLinearFunction :: forall b f u v du dv. (Eq b, BVector b u du, BVector b v dv) => LinearFunction b f u v du dv => f -> dv -> u -> Bool
_testLinearFunction f dv u = lhs == rhs
    where lhs = (dv ⊗ f :: du) ⊗ u :: b
          rhs = dv ⊗ (f ⊗ u) :: b

data AFunction b u du v dv = AFunction
    { funcFwd  :: u -> v
    , funcBack :: dv -> du
    }

instance TensorProduct (AFunction b u du v dv) u v where
    f ⊗ x = funcFwd f x

instance TensorProduct dv (AFunction b u du v dv) du where
    x ⊗ f = funcBack f x

transposeFunc :: AFunction b u v du dv -> AFunction b dv du v u
transposeFunc (AFunction f g) = AFunction g f

instance (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => LinearFunction b (AFunction b u du v dv) u v du dv

instance LinearFunction' b (AFunction b) where
    transpose = transposeFunc
    transposeC = Sub Dict

type family CommonType a b :: Type where
    CommonType a a = a

data Pair a b = Pair a b
data CoPair a b = CoPair a b

data PairFunc f g v = PairFunc f g

instance (AdditiveGroup f, AdditiveGroup g) => AdditiveGroup (PairFunc f g v) where
    zeroV = PairFunc zeroV zeroV
    negateV (PairFunc f g) = PairFunc (negateV f) (negateV g)
    PairFunc f1 g1 ^+^ PairFunc f2 g2 = PairFunc (f1 ^+^ f2) (g1 ^+^ g2)

instance
  ( AdditiveGroup v
  , TensorProduct f a v
  , TensorProduct g b v
  )
  => TensorProduct (PairFunc f g v) (Pair a b) v where
    (PairFunc f g) ⊗ (Pair a b) = (f ⊗ a) ^+^ (g ⊗ b)

instance
  ( AdditiveGroup v
  , TensorProduct a f v
  , TensorProduct b g v
  )
  => TensorProduct (CoPair a b) (PairFunc f g v) v where
    (CoPair a b) ⊗ (PairFunc f g) = (a ⊗ f) ^+^ (b ⊗ g)

instance
  ( TensorProduct du u b
  , TensorProduct dv v b
  , AdditiveGroup b
  )
  => TensorProduct (CoPair du dv) (Pair u v) b where
    CoPair da db ⊗ Pair a b = (da ⊗ a) ^+^ (db ⊗ b)

instance BVector b u du => BVector b (u, v) (du, dv) where

instance BVector b u du => TensorProduct (du, dv) (u, v) b where
    (du, _) ⊗ (u, _) = du ⊗ u

data GetFirst b u v du dv = GetFirst
instance TensorProduct (GetFirst b u v du dv) (u, v) u where
    GetFirst ⊗ (u, _) = u
instance AdditiveGroup dv => TensorProduct du (GetFirst b u v du dv) (du, dv) where
    du ⊗ GetFirst = (du, zeroV)

data SetFirst b u v du dv = SetFirst
instance AdditiveGroup v => TensorProduct (SetFirst b u v du dv) u (u, v) where
    SetFirst ⊗ u = (u, zeroV)
instance TensorProduct (du, dv) (SetFirst b u v du dv) du where
    (du, _) ⊗ SetFirst = du


--  (TensorProduct f u v, TensorProduct (Dual b v) f (Dual b u)) 
-- instance LinearFunction b (GetFirst b u v) (u, v) v where

{-
instance TensorProduct (GetFirst u v) (u, v) where
    type (GetFirst u v) ⊗ (u, v) = u
    GetFirst ⊗ (u, _) = u

instance TensorProduct (GetSecond u v) (u, v) where
    type (GetSecond u v) ⊗ (u, v) = v
    GetSecond ⊗ (_, v) = v
-}


{-
newtype ScalarLinearFunc a = ScalarLinearFunc a
newtype ScalarGrad a = ScalarGrad a

instance Num a => TensorProduct (ScalarLinearFunc a) a where
    type (ScalarLinearFunc a) ⊗ a = a
    (ScalarLinearFunc f) ⊗ x = f * x

instance Num a => TensorProduct a (ScalarGrad a) where
    type a ⊗ (ScalarGrad a) = a
    x ⊗ (ScalarLinearFunc f) = x * f

instance Num a => TensorProduct (ScalarGrad a) (Scalar)
-}
