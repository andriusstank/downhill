{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language TypeApplications #-}
{-# language ExplicitForAll #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
where
import Tensor(TensorProduct(..), LinearFunction, AFunction, transposeFunc)
import Data.VectorSpace (AdditiveGroup, sumV)
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv

data Expr a da b v dv where
    Variable :: Expr a da b a da
    -- Func :: LinearFunction b f u v du dv => f -> Expr a da b u du -> Expr a da b v dv
    Func :: (AdditiveGroup v, AdditiveGroup du) => AFunction b u v du dv -> Expr a da b u du -> Expr a da b v dv
    Sum :: AdditiveGroup v => [Expr a da b v dv] -> Expr a da b v dv

-- Evaluate
instance TensorProduct (Expr a da b v dv) a v where
    expr ⊗ a = case expr of
        Variable -> a
        Func f x -> f ⊗ (x ⊗ a)
        Sum xs -> sumV [x ⊗ a | x <- xs]

-- Substitute
instance TensorProduct (Expr x dx b v dv) (Expr a da b x dx) (Expr a da b v dv) where
    expr ⊗ x = case expr of
        Variable -> x
        Func f y -> Func f (y ⊗ x)
        Sum ys -> Sum [y ⊗ x | y <- ys]

-- Reverse mode evaluation
instance AdditiveGroup da => TensorProduct dv (Expr a da b v dv) da where
    dv ⊗ expr = case expr of
        Variable -> dv
        Func f x -> (dv ⊗ f) ⊗ x
        Sum xs -> sumV [dv ⊗ x | x <- xs]

-- >>> 1

-- Func :: LinearFunction b f u v du dv => f -> Expr a da b u du -> Expr a da b v dv

_flipFunc :: forall b u v du dv a da. (AdditiveGroup da, AdditiveGroup dv, AdditiveGroup du, AdditiveGroup v) => AFunction b u v du dv -> Expr a da b u du -> Expr dv v b da a
_flipFunc f x = x' ⊗ f'
    where x' :: Expr du u b da a
          f' :: Expr dv v b du u
          x' = flipExpr x
          f' = Func (transposeFunc f) Variable

          -- expr' ::
          -- expr' = Func @b @(Flip b f u v) @du @dv @v @u f' (flipExpr x)
-- Func (Flip f) (flipExpr x)

flipExpr :: (AdditiveGroup da, AdditiveGroup dv) => Expr a da b v dv -> Expr dv v b da a
flipExpr = \case
    Variable -> Variable
    Func f x -> _flipFunc f x
    Sum xs -> Sum (flipExpr <$> xs)
