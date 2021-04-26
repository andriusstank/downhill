{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Coolness
where
import Data.AdditiveGroup (sumV, AdditiveGroup)
import Tensor (AFunction, Bilinear(..), Vec(..), Bilinear'', transposeFunc)
import Data.Constraint (Dict(Dict), (:-)(Sub))

{-
class Bilinear u v w | u v -> w where
  (✕) :: u -> v -> w

class Bilinear u v (u✕v) => Bilinear' u v where
  type u ✕ v :: Type
-}

data Expr a da v dv where
    Variable :: Expr a da a da
    Func :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction u du v dv -> Expr a da u du -> Expr a da v dv
    Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv

-- Evaluate
instance Bilinear (Expr a da v dv) (Vec a) where
    type Expr a da v dv ✕ Vec a = Vec v
    expr ✕ a = case expr of
        Variable -> a
        Func f x -> f ✕ (x ✕ a)
        Sum xs -> sumV [x ✕ a | x <- xs]

-- Substitute
instance Bilinear (Expr x dx v dv) (Expr a da x dx) where
    type Expr x dx v dv ✕ Expr a da x dx = Expr a da v dv
    expr ✕ x = case expr of
        Variable -> x
        Func f y -> Func f (y ✕ x)
        Sum ys -> Sum [y ✕ x | y <- ys]

-- Reverse mode evaluation
instance AdditiveGroup da => Bilinear (Vec dv) (Expr a da v dv) where
    type Vec dv ✕ (Expr a da v dv) = Vec da
    dv ✕ expr = case expr of
        Variable -> dv
        Func f x -> (dv ✕ f) ✕ x
        Sum xs -> sumV [dv ✕ x | x <- xs]

instance (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => LinearFunction (Expr u du v dv) u v du dv


-- f: u -> v
-- dv ~ Dual b v
-- du ~ Dual b u
--  f ✕ u ::  v
-- dv ✕ f :: du
-- (dv ✕ f) ✕ u == dv ✕ (f ✕ u)
class
  ( AdditiveGroup u
  , AdditiveGroup v
  , AdditiveGroup du
  , AdditiveGroup dv
  , Bilinear'' f (Vec u) (Vec v)
  , Bilinear'' (Vec dv) f (Vec du)
  ) => LinearFunction f u v du dv where

class Bilinear'' (Vec dv) (Vec v) (Vec b) => BVector b v dv where

class
  ( 
  ) => LinearFunction' f where
    transpose :: LinearFunction (f u du v dv) u v du dv => f u du v dv -> f dv v du u
    transposeC :: LinearFunction (f u du v dv) u v du dv :- LinearFunction (f dv v du u) dv du v u

instance (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => LinearFunction (AFunction u du v dv) u v du dv

instance LinearFunction' AFunction where
    transpose = transposeFunc
    transposeC = Sub Dict

instance LinearFunction' Expr where
    transpose = \case
        Variable -> Variable
        Func f x -> x' ✕ f'
            where x' = transpose x
                  f' = Func (transpose f) Variable
        Sum xs -> Sum (transpose <$> xs)
    transposeC = Sub Dict
