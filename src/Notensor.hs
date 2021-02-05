{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Notensor
( BasicVector(..), BasicVectors, FullVector(..), FullVectors
, AFunction1(..), AFunction2(..), ProdVector(..)
, mkAFunction2
, identityFunc, negateFunc, scaleFunc
, transposeFunc2
, fstF, fstF1, sndF, sndF1, toFunc1, intoFst, intoSnd
) where
import Data.VectorSpace (VectorSpace(Scalar))
import Tensor (TensorProduct((⊗)))
import Data.Maybe (catMaybes)
import EType

class BasicVector v => ProdVector v where
    zeroBuilder :: VecBuilder v
    identityBuilder :: v -> VecBuilder v

class (ProdVector v, ProdVector v) => FullVector v where
    negateBuilder :: v -> VecBuilder v
    scaleBuilder :: Scalar v -> v -> VecBuilder v

instance (BasicVector a, BasicVector b) => BasicVector (a, b) where
    type VecBuilder (a, b) = (Maybe (VecBuilder a), Maybe (VecBuilder b))
    sumBuilder xs =
        ( sumBuilder (catMaybes (fst <$> xs))
        , sumBuilder (catMaybes (snd <$> xs))
        )

instance (ProdVector a, ProdVector b) => ProdVector (a, b) where
    zeroBuilder = (Nothing, Nothing)
    identityBuilder (x, y) = (Just (identityBuilder x), Just (identityBuilder y))

fstF1 :: ProdVector du => AFunction1 (du, dv) du
fstF1 = AFunction1 back
    where back x = (Just (identityBuilder x), Nothing)

intoFst :: ProdVector du => AFunction1 du (du, dv)
intoFst = AFunction1 fwd
    where fwd (x, _) = identityBuilder x

fstF :: (ProdVector u, ProdVector du) => AFunction2 (u, v) (du, dv) u du
fstF = AFunction2 fwd back
    where fwd (x, _) = identityBuilder x
          back x = (Just (identityBuilder x), Nothing)

sndF1 :: ProdVector dv => AFunction1 (du, dv) dv
sndF1 = AFunction1 back
    where back x = (Nothing, Just (identityBuilder x))

intoSnd :: ProdVector dv => AFunction1 dv (du, dv)
intoSnd = AFunction1 fwd
    where fwd (_, x) = identityBuilder x

sndF :: (ProdVector v, ProdVector dv) => AFunction2 (u, v) (du, dv) v dv
sndF = AFunction2 fwd back
    where fwd (_, x) = identityBuilder x
          back x = (Nothing, Just (identityBuilder x))

transposeFunc2 :: AFunction2 u du v dv -> AFunction2 dv v du u
transposeFunc2 (AFunction2 fwd back) = AFunction2 back fwd

type BasicVectors v dv = (BasicVector v, BasicVector dv)
type FullVectors v dv = (FullVector v, FullVector dv, Scalar v ~ Scalar dv)

data AFunction1 du dv = AFunction1 { backF1 :: dv -> VecBuilder du }

data AFunction2 u du v dv = AFunction2
    { fwdF :: u -> VecBuilder v
    , backF :: dv -> VecBuilder du
    }

toFunc1 :: AFunction2 u du v dv -> AFunction1 du dv
toFunc1 (AFunction2 _ back) = AFunction1 back

-- TODO: review all uses
mkAFunction2 :: (FullVector v, FullVector du) => (u->v) -> (dv->du) -> AFunction2 u du v dv
mkAFunction2 fwd back = AFunction2 (identityBuilder . fwd) (identityBuilder . back)

negateFunc :: FullVector du => AFunction1 du du
negateFunc = AFunction1 negateBuilder

identityFunc :: FullVector du => AFunction1 du du
identityFunc = AFunction1 identityBuilder


scaleFunc :: FullVector du => Scalar du -> AFunction1 du du
scaleFunc a = AFunction1 (scaleBuilder a)

-- TODO: remove TensorProduct instances
instance BasicVector v => TensorProduct (AFunction2 u du v dv) u v where
    (AFunction2 f _) ⊗ x = sumBuilder [f x]

instance BasicVector du => TensorProduct dv (AFunction2 u du v dv) du where
    x ⊗ (AFunction2 _ f) = sumBuilder [f x]

{-
data AFunction u du v dv where
    IndentityFunc :: AFunction u du u du
    NegateFunc :: (AdditiveGroup u, AdditiveGroup du) => AFunction u du u du
    ScaleFunc :: forall a v dv. (VectorSpace v, VectorSpace dv, a ~ Scalar v, a ~ Scalar dv) => a -> AFunction v dv v dv
    BlackBoxFunc :: (u -> v) -> (dv -> du) -> AFunction u du v dv
-}

instance BasicVector Integer where
    type VecBuilder Integer = Integer
    sumBuilder = Prelude.sum

instance ProdVector Integer where
    zeroBuilder = 0
    identityBuilder = Prelude.id

instance FullVector Integer where
    negateBuilder = Prelude.negate
    scaleBuilder a = (a *)