{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Coolness
where
import Data.AdditiveGroup (sumV, AdditiveGroup)
import Tensor (LinearFunction, LinearFunction'(transpose, transposeC), TensorProduct((⊗)), AFunction)
import Data.Constraint (Dict(Dict), (:-)(Sub))

data Expr a da v dv where
    Variable :: Expr a da a da
    Func :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction u du v dv -> Expr a da u du -> Expr a da v dv
    Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv

-- Evaluate
instance TensorProduct (Expr a da v dv) a v where
    expr ⊗ a = case expr of
        Variable -> a
        Func f x -> f ⊗ (x ⊗ a)
        Sum xs -> sumV [x ⊗ a | x <- xs]

-- Substitute
instance TensorProduct (Expr x dx v dv) (Expr a da x dx) (Expr a da v dv) where
    expr ⊗ x = case expr of
        Variable -> x
        Func f y -> Func f (y ⊗ x)
        Sum ys -> Sum [y ⊗ x | y <- ys]

-- Reverse mode evaluation
instance AdditiveGroup da => TensorProduct dv (Expr a da v dv) da where
    dv ⊗ expr = case expr of
        Variable -> dv
        Func f x -> (dv ⊗ f) ⊗ x
        Sum xs -> sumV [dv ⊗ x | x <- xs]

instance (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => LinearFunction (Expr u du v dv) u v du dv

instance LinearFunction' Expr where
    transpose = \case
        Variable -> Variable
        Func f x -> x' ⊗ f'
            where x' = transpose x
                  f' = Func (transpose f) Variable
        Sum xs -> Sum (transpose <$> xs)
    transposeC = Sub Dict
