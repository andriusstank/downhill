{-# LANGUAGE TypeFamilies #-}
module Notensor
( AdVector(..)
, AFunction2(..)
) where
import Data.Kind (Type)

class AdVector v where
    type VecBuilder v :: Type
    -- identityBuilder :: v -> VecBuilder v
    sumBuilder :: [VecBuilder v] -> v

data AFunction2 u du v dv = AFunction2
    { fwdF :: u -> VecBuilder v
    , backF :: dv -> VecBuilder du
    }


{-
data AFunction u du v dv where
    IndentityFunc :: AFunction u du u du
    NegateFunc :: (AdditiveGroup u, AdditiveGroup du) => AFunction u du u du
    ScaleFunc :: forall a v dv. (VectorSpace v, VectorSpace dv, a ~ Scalar v, a ~ Scalar dv) => a -> AFunction v dv v dv
    BlackBoxFunc :: (u -> v) -> (dv -> du) -> AFunction u du v dv
-}
