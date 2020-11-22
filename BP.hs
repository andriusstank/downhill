{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language FlexibleContexts, FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused #-}

module BP where
import Data.STRef
import Data.Kind
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad.ST

data Vector a = Vector a a
    deriving Show
data Grad a = Grad a a
    deriving Show
data Matrix a = Matrix a a a a
    deriving Show
data MatrixT a = MatrixT a a a a
    deriving Show

class LinearFunc f u v | f -> u v where
    eval :: f -> (u -> v)

class TensorProduct u v where
    type u ⊗ v :: Type
    (⊗) :: u -> v -> u ⊗ v

newtype LeftMul u v a = LeftMul u
newtype RightMul u v a = RightMul v

instance (TensorProduct u v, u⊗v ~ a) => LinearFunc (LeftMul u v a) v a where
    eval (LeftMul u) = (u ⊗)

instance (TensorProduct u v, u⊗v ~ a) => LinearFunc (RightMul u v a) u a where
    eval (RightMul v) = (⊗ v)

instance Num a => TensorProduct (Vector a) (Grad a) where
    type (Vector a) ⊗ (Grad a) = a
    Vector x y ⊗ Grad dx dy = x*dx + y*dy

-- class (v ⊗ dv ~ a, dv ⊗ v ~ a) => LinearSpace a v dv where

{-
instance Num a => LinearFunc (Grad a) (Vector a) a where
    eval (Grad dx dy) (Vector x y) = x*dx + y*dy

instance Num a => LinearFunc (Matrix a) (Vector a) (Vector a) where
    eval (Matrix c11 c12 c21 c22) (Vector v1 v2) = Vector (c11*v1 + c12*v2) (c21*v1+c22*v2)

instance Num a => LinearFunc (MatrixT a) (Grad a) (Grad a) where
    eval (MatrixT c11 c21 c12 c22) (Grad v1 v2) = Grad (c11*v1 + c12*v2) (c21*v1+c22*v2)
-}

-- data SomeLinearFunc a b = forall f. LinearFunc f a b => SomeLinearFunc f

-- class LinearFunc g v a => Gradient g a v | g -> a v where

--class LinearFunc (GradOf v) v a => BackpropValue v a where
--    type GradOf v :: Type
{-
class
  ( LinearFunc (Fwd f) u v
  , LinearFunc (Back f) dv du
  , LinearFunc du u a
  , LinearFunc dv v a
  ) => BackpropFunc f a u du v dv  | f -> a u du v dv where
    type Fwd f :: Type
    type Back f :: Type
    fwd :: f -> Fwd f
    back :: f -> Back f
-}

-- dv ⊗ (f ⊗ u) == (dv ⊗ f) ⊗ u
class
  ( TensorProduct f u
  , TensorProduct dv f
  , v ~ (f ⊗ u)
  , (dv ⊗ f) ~ du
  , du ⊗ u ~ a
  , dv ⊗ v ~ a
  ) => BackpropFunc f a u v du dv where
--instance TensorProduct 

data BVarLinearPart a da b v dv where
    Root :: BVarLinearPart v dv b v dv
    Const :: BVarLinearPart a da b v dv
    Expression :: [BVarParent a da b v dv] -> BVarLinearPart a da b v dv

data BVarParent a da b v dv where
    BVarParent :: forall f a da b u v du dv. BackpropFunc f b u v du dv => BVarLinearPart a da b u du -> f -> BVarParent a da b v dv

data BVar a da b v dv = BVar v (BVarLinearPart a da b v dv)

backprop :: forall a da b v dv. AdditiveGroup da => dv -> BVarLinearPart a da b v dv -> da
backprop dv = \case
    Root -> dv
    Const -> zeroV
    Expression parents -> sumV (backpropEdge <$> parents)
        where backpropEdge :: BVarParent a da b v dv -> da
              backpropEdge (BVarParent u f) = backprop (dv ⊗ f) u

{-
test1 :: forall f a u du v dv. BackpropFunc f a u du v dv => f -> u -> dv -> a
test1 bf u dv = y
    where f = fwd bf
          fu = eval f u :: v
          y = eval dv (eval f u) :: a

test2 :: forall f a u du v dv. BackpropFunc f a u du v dv => f -> u -> dv -> a
test2 bf u dv = y
    where f = back bf
          du = eval f dv :: du
          y = eval du u
-}

{-
instance Num a => BackpropFunc (Matrix a) (MatrixT a) a (Vector a) (Grad a) (Vector a) (Grad a) where
    transposeF (Matrix a b c d) = MatrixT a b c d



v1 = Vector 2 3 :: Vector Int
g1 = Grad 17 19 :: Grad Int
m1 = Matrix 5 7 11 13 :: Matrix Int

-- >>> test1 @(Matrix Int) @(MatrixT Int) m1 v1 g1
-- 1686

-- >>> test2 @(Matrix Int) @(MatrixT Int) m1 v1 g1
-- 1686

-}
{-
type family GradOf v :: Type

class LinearFunc f a u v | f -> a u v where
    type FT f :: Type
    eval :: f -> (u -> v)
    transpose :: f -> FT f

data AffineFunc p f = AffineFunc p f
    deriving Show

evalAffine :: (AffineSpace p, Diff p ~ v, LinearFunc f a u v) => AffineFunc p f -> u -> p
evalAffine (AffineFunc y0 dy_dx) dx = y0 .+^ dy
    where dy = eval dy_dx dx

newtype ScalarLinearFunc a = ScalarLinearFunc a
    deriving Show

newtype TransposedLinearFunc a = TransposedLinearFunc a

newtype ScalarGrad a = ScalarGrad a

instance Num a => LinearFunc (TransposedLinearFunc a) a (ScalarGrad a) (ScalarGrad a) where
    eval (TransposedLinearFunc x) (ScalarGrad y) = ScalarGrad (x*y)

instance Num a => LinearFunc (ScalarLinearFunc a) a a a where
    eval (ScalarLinearFunc x) = (x*)

type ScalarAffineFunc a = AffineFunc a (ScalarLinearFunc a)
-}
