{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Coolness
where
import Data.AdditiveGroup (sumV, AdditiveGroup)


class TensorMul u v w  | u v -> w where
  (✕) :: u -> v -> w

data BlackBoxFunc u du v dv = BlackBoxFunc (u -> v) (dv -> du)

instance TensorMul (BlackBoxFunc u du v dv) u v where
    (BlackBoxFunc f _) ✕ v = f v

instance TensorMul dv (BlackBoxFunc u du v dv) du where
    v ✕ (BlackBoxFunc _ f) = f v


data Expr a da v dv where
    Var :: Expr a da a da
    Func :: BlackBoxFunc u du v dv -> Expr a da u du -> Expr a da v dv
    Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv

-- Evaluate
instance TensorMul (Expr a da v dv) a v where
    expr ✕ a = case expr of
        Var -> a
        Func f v -> f ✕ (v ✕ a)  -- (f ✕ v) ✕ a -> f ✕ (v ✕ a)
        Sum vs -> sumV [v ✕ a | v <- vs]


-- Reverse mode evaluation
instance AdditiveGroup da => TensorMul dv (Expr a da v dv) da where
    dv ✕ expr = case expr of
        Var -> dv
        Func f v -> (dv ✕ f) ✕ v
        Sum vs -> sumV [dv ✕ v | v <- vs]

-- Substitute
instance TensorMul (Expr x dx v dv) (Expr a da x dx) (Expr a da v dv) where
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
