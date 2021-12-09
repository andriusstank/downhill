{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Coolness where

import Data.AdditiveGroup (AdditiveGroup, sumV)
import Data.Kind (Type)

{-| Laws:

Associativity:

(u ✕ v) ✕ w = u ✕ (v ✕ w)

Distribution over `^+^`:

(u ^+^ v) ✕ w = u ✕ w ^+^ v ✕ w
w ✕ (u ^+^ v) = w ✕ u ^+^ w ✕ v

Multiplication by scalar:

a *^ (u ✕ v) = (a *^ u) ✕ v = u ✕ (a *^ v)

-}
class TensorMul u v where
  type u ✕ v :: Type
  (✕) :: u -> v -> u ✕ v

infixl 7 ✕

newtype Vec x = Vec {unVec :: x}
  deriving (Show)
  deriving (AdditiveGroup) via x

newtype Cov x = Cov {unCov :: x}
  deriving (Show)
  deriving (AdditiveGroup) via x

data PrimFunc u du v dv = PrimFunc
  { fwdFun :: u -> v,
    backFun :: dv -> du
  }

instance TensorMul (PrimFunc u du v dv) (Vec u) where
  type (PrimFunc u du v dv) ✕ (Vec u) = Vec v
  (PrimFunc f _) ✕ Vec v = Vec (f v)

instance TensorMul (Cov dv) (PrimFunc u du v dv) where
  type (Cov dv) ✕ (PrimFunc u du v dv) = Cov du
  Cov v ✕ (PrimFunc _ f) = Cov (f v)

data Expr a da v dv where
  Var :: Expr a da a da
  Func :: PrimFunc u du v dv -> Expr a da u du -> Expr a da v dv
  Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv

-- Evaluate
instance TensorMul (Expr a da v dv) (Vec a) where
  type Expr a da v dv ✕ Vec a = Vec v
  expr ✕ a = case expr of
    Var -> a -- Var is identity function
    Func f v -> f ✕ (v ✕ a) -- Func f v = f ✕ v
    Sum vs -> sumV [v ✕ a | v <- vs]

-- Reverse mode evaluation
instance AdditiveGroup da => TensorMul (Cov dv) (Expr a da v dv) where
  type Cov dv ✕ (Expr a da v dv) = Cov da
  dv ✕ expr = case expr of
    Var -> dv
    Func f v -> (dv ✕ f) ✕ v
    Sum vs -> sumV [dv ✕ v | v <- vs]

-- Substitute
instance TensorMul (Expr x dx v dv) (Expr a da x dx) where
  type Expr x dx v dv ✕ Expr a da x dx = Expr a da v dv
  expr ✕ v = case expr of
    Var -> v
    Func f y -> Func f (y ✕ v)
    Sum ys -> Sum [y ✕ v | y <- ys]

class (TransposeOf (TransposeOf a) ~ a) => Transposable a where
  type TransposeOf a :: Type
  transpose :: a -> TransposeOf a

-- Laws:
-- tranpose (transpose a) = a
-- transpose (a ✕ b) = transpose b ✕ transpose a

instance Transposable (Vec v) where
  type TransposeOf (Vec v) = Cov v
  transpose (Vec v) = Cov v

instance Transposable (Cov v) where
  type TransposeOf (Cov v) = Vec v
  transpose (Cov v) = Vec v

instance Transposable (PrimFunc u v du dv) where
  type TransposeOf (PrimFunc u v du dv) = PrimFunc dv du v u
  transpose (PrimFunc f g) = PrimFunc g f

instance AdditiveGroup da => Transposable (Expr a da v dv) where
  type TransposeOf (Expr a da v dv) = Expr dv v da a
  transpose =  \case
    Var -> Var
    Func f v -> v' ✕ f'
      where
        v' = transpose v
        f' = Func (transpose f) Var
    Sum xs -> Sum (transpose <$> xs)
