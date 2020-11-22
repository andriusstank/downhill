{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Expr
where
import Tensor(TensorProduct(..), LinearFunction, Flip)
import Data.VectorSpace (AdditiveGroup, sumV)
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv

data Expr a da b v dv where
    Variable :: Expr a da b a da
    Func :: LinearFunction b f u v du dv => f -> Expr a da b u du -> Expr a da b v dv
    Sum :: AdditiveGroup v => [Expr a da b v dv] -> Expr a da b v dv

evalExpr :: Expr a da b v dv -> a -> v
evalExpr expr x = case expr of
    Variable -> x
    Func f y -> f ⊗ (y ⊗ x)
    Sum ys -> sumV [y ⊗ x | y <- ys]

backprop :: AdditiveGroup da => dv -> Expr a da b v dv -> da
backprop dv = \case
    Variable -> dv
    Func f x -> (dv ⊗ f) ⊗ x
    Sum xs -> sumV [dv ⊗ x | x <- xs]

instance TensorProduct (Expr a da b v dv) a v where
    expr ⊗ a = evalExpr expr a

instance AdditiveGroup da => TensorProduct dv (Expr a da b v dv) da where
    dv ⊗ expr = backprop dv expr

{-
flipExpr :: Expr a da b v dv -> Expr dv v b da a
flipExpr = \case
    Variable -> Variable
    Func f x -> Func (Flip f) (flipExpr x)
    Sum x -> Sum (flipExpr <$> x)
-}
