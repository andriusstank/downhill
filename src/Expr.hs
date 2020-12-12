{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
where
--import Tensor(TensorProduct(..), LinearFunction, AFunction, transposeFunc)
import Tensor
import Data.VectorSpace (AdditiveGroup, sumV)
import Data.Constraint
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv

data Expr b a v da dv where
    Variable :: Expr b a a da da
    -- Func :: LinearFunction b f u v du dv => f -> Expr a da b u du -> Expr a da b v dv
    Func :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction b u v du dv -> Expr b a u da du -> Expr b a v da dv
    Sum :: AdditiveGroup v => [Expr b a v da dv] -> Expr b a v da dv

type Expr' b a da v dv = Expr b a v da dv

-- Evaluate
instance TensorProduct (Expr b a v da dv) a v where
    expr ⊗ a = case expr of
        Variable -> a
        Func f x -> f ⊗ (x ⊗ a)
        Sum xs -> sumV [x ⊗ a | x <- xs]

-- Substitute
instance TensorProduct (Expr b x v dx dv) (Expr b a x da dx) (Expr b a v da dv) where
    expr ⊗ x = case expr of
        Variable -> x
        Func f y -> Func f (y ⊗ x)
        Sum ys -> Sum [y ⊗ x | y <- ys]

-- Reverse mode evaluation
instance AdditiveGroup da => TensorProduct dv (Expr b a v da dv) da where
    dv ⊗ expr = case expr of
        Variable -> dv
        Func f x -> (dv ⊗ f) ⊗ x
        Sum xs -> sumV [dv ⊗ x | x <- xs]

instance (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => LinearFunction b (Expr b u v du dv) u v du dv

instance LinearFunction' b (Expr b) where
    transpose = \case
        Variable -> Variable
        Func f x -> x' ⊗ f'
            where x' = transpose x
                  f' = Func (transpose f) Variable
        Sum xs -> Sum (transpose <$> xs)
    transposeC = Sub Dict
-- >>> 1

-- Func :: LinearFunction b f u v du dv => f -> Expr a da b u du -> Expr a da b v dv

