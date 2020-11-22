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
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Tensor (
    TensorProduct(..),
    PairFunc(..)
)
where
import Data.Kind (Type)
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import GHC.Generics (Generic)

class TensorProduct u v w | u v -> w where
  (⊗) :: u -> v -> w

class TensorProduct u v (u⊗v) => TensorProduct' u v where
  type u ⊗ v :: Type

class TensorProduct (Dual b v) v b => BVector b v where
    data Dual b v :: Type


-- f: u -> v
-- dv ~ Dual b v
-- du ~ Dual b u
--  f ⊗ u ::  v
-- dv ⊗ f :: du
-- (dv ⊗ f) ⊗ u == dv ⊗ (f ⊗ u)
class (TensorProduct f u v, TensorProduct (Dual b v) f (Dual b u)) => LinearFunction b f u v | f -> b where

_testLinearFunction :: forall b f u v dv. (Eq b, dv ~ Dual b v, BVector b u, BVector b v) => LinearFunction b f u v => f -> dv -> u -> Bool
_testLinearFunction f dv u = lhs == rhs
    where lhs = (dv ⊗ f :: Dual b u) ⊗ u :: b
          rhs = dv ⊗ (f ⊗ u) :: b

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

data GetFirst b u v = GetFirst
data GetSecond b u v = GetSecond

instance BVector b u => TensorProduct (Dual b (u, v)) (u, v) b where
    DualPair du _ ⊗ (u, _) = du ⊗ u
instance BVector b u => BVector b (u, v) where
    data Dual b (u, v) = DualPair (Dual b u) (Dual b v)

instance TensorProduct (GetFirst b u v) (u, v) u where
    GetFirst ⊗ (u, _) = u
instance AdditiveGroup (Dual b v) => TensorProduct (Dual b u) (GetFirst b u v) (Dual b (u, v)) where
    du ⊗ GetFirst = DualPair du zeroV

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
