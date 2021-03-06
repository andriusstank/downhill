{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module BVar.Vec
where
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import Data.Kind (Type)
import Affine (AffineFunc(AffineFunc))
import Tensor (Bilinear(..))

type family GradOf a :: Type

newtype VecBVar a = BVar (AffineFunc a (GradOf a))

constant :: AdditiveGroup (GradOf a) => a -> VecBVar a
constant x = BVar (AffineFunc x zeroV)

var :: Num (GradOf a) => a -> VecBVar a
var x = BVar (AffineFunc x 1)

instance AdditiveGroup (VecBVar a) where
    (^+^) = undefined
    (^-^) = undefined
    zeroV = undefined
    negateV = undefined

instance
    ( VectorSpace (GradOf a)
    , VectorSpace a
    , Scalar (GradOf a) ~ Scalar a
    , GradOf a ~ (GradOf (Scalar a) ✕ a)
    , Bilinear (GradOf (Scalar a)) a
    ) => VectorSpace (VecBVar a) where
    type Scalar (VecBVar a) = VecBVar (Scalar a)
    BVar (AffineFunc a da) *^ BVar (AffineFunc v dv) = BVar (AffineFunc (a*^v) (part1 ^+^ part2))
        where part1 :: GradOf a
              part1 = a *^ dv
              part2 :: GradOf a
              part2 = da ✕ v

