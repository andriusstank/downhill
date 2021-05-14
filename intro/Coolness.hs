{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}


{-# LANGUAGE DerivingVia #-}
module Coolness
where
import Data.AdditiveGroup (sumV, AdditiveGroup)
--import Tensor (AFunction, Bilinear(..), Vec(..), Bilinear'', transposeFunc)
import Data.Kind (Type)


class Bilinear u v where
  type u ✕ v :: Type
  (✕) :: u -> v -> u ✕ v

newtype Vec dx = Vec { unVec :: dx }
    deriving Show
    deriving AdditiveGroup via dx

--type family Vec dx where
    --Vec dx = dx

data AFunction u du v dv where
    BlackBoxFunc :: (u -> v) -> (dv -> du) -> AFunction u du v dv

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

instance Bilinear (AFunction u du v dv) (Vec u) where
    type (AFunction u du v dv) ✕ (Vec u) = Vec v
    (BlackBoxFunc f _) ✕ Vec x = Vec (f x)

instance Bilinear (Vec dv) (AFunction u du v dv) where
    type (Vec dv) ✕ (AFunction u du v dv) = Vec du
    Vec x ✕ (BlackBoxFunc _ f) = Vec (f x)

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

transposeFunc :: AFunction u v du dv -> AFunction dv du v u
transposeFunc = \case
    BlackBoxFunc f g -> BlackBoxFunc g f

transposeExpr :: AdditiveGroup da => Expr a da v dv -> Expr dv v da a
transposeExpr = \case
    Variable -> Variable
    Func f x -> x' ✕ f'
        where x' = transposeExpr x
              f' = Func (transposeFunc f) Variable
    Sum xs -> Sum (transposeExpr <$> xs)
