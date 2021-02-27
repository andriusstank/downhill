{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# language ScopedTypeVariables #-}

module BVar.Num
where
import Affine (AffineFunc(AffineFunc))
import Data.Kind (Type)
import Data.VectorSpace (zeroV, AdditiveGroup(..), VectorSpace(..))
import Tensor (Bilinear(..))

type family GradOf a :: Type

newtype BVar a = BVar (AffineFunc a (GradOf a))

constant :: AdditiveGroup (GradOf a) => a -> BVar a
constant x = BVar (AffineFunc x zeroV)

var :: Num (GradOf a) => a -> BVar a
var x = BVar (AffineFunc x 1)


instance AdditiveGroup (BVar a) where
    (^+^) = undefined
    (^-^) = undefined
    zeroV = undefined
    negateV = undefined

instance (VectorSpace (GradOf a), VectorSpace a, Scalar (GradOf a) ~ Scalar a, GradOf a ~ (GradOf (Scalar a) ✕ a), Bilinear (GradOf (Scalar a)) a) => VectorSpace (BVar a) where
    type Scalar (BVar a) = BVar (Scalar a)
    BVar (AffineFunc a da) *^ BVar (AffineFunc v dv) = BVar (AffineFunc (a*^v) (part1 ^+^ part2))
        where part1 :: GradOf a
              part1 = a *^ dv
              part2 :: GradOf a
              part2 = da ✕ v

