{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}

module Coolness
where
import Data.AdditiveGroup (sumV, AdditiveGroup)
import Data.Kind (Type)


class TensorMul u v where
  type u ✕ v :: Type
  (✕) :: u -> v -> u ✕ v

newtype Vec dx = Vec { unVec :: dx }
    deriving Show
    deriving AdditiveGroup via dx


data BlackBoxFunc u du v dv = BlackBoxFunc (u -> v) (dv -> du)

instance TensorMul (BlackBoxFunc u du v dv) (Vec u) where
    type (BlackBoxFunc u du v dv) ✕ (Vec u) = Vec v
    (BlackBoxFunc f _) ✕ Vec v = Vec (f v)

instance TensorMul (Vec dv) (BlackBoxFunc u du v dv) where
    type (Vec dv) ✕ (BlackBoxFunc u du v dv) = Vec du
    Vec v ✕ (BlackBoxFunc _ f) = Vec (f v)


data Expr a da v dv where
    Var :: Expr a da a da
    Func :: BlackBoxFunc u du v dv -> Expr a da u du -> Expr a da v dv
    Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv

-- Evaluate
instance TensorMul (Expr a da v dv) (Vec a) where
    type Expr a da v dv ✕ Vec a = Vec v
    expr ✕ a = case expr of
        Var -> a
        Func f v -> f ✕ (v ✕ a)
        Sum vs -> sumV [v ✕ a | v <- vs]


-- Reverse mode evaluation
instance AdditiveGroup da => TensorMul (Vec dv) (Expr a da v dv) where
    type Vec dv ✕ (Expr a da v dv) = Vec da
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


transposeFunc :: BlackBoxFunc u v du dv -> BlackBoxFunc dv du v u
transposeFunc (BlackBoxFunc f g) =  BlackBoxFunc g f

transposeExpr :: AdditiveGroup da => Expr a da v dv -> Expr dv v da a
transposeExpr = \case
    Var -> Var
    Func f v -> v' ✕ f'
        where v' = transposeExpr v
              f' = Func (transposeFunc f) Var
    Sum xs -> Sum (transposeExpr <$> xs)
