{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Notensor
( BasicVector(..), BasicVectors, FullVector(..), FullVectors, Dense(..)
, NumBuilder(..)
, BackFunc(..), FwdFunc(..), flipFunc1, ProdVector(..), Transpose(..)
, LinearEdge(..)
, fstF1, sndF1, intoFst, intoSnd
) where
import Data.Kind (Type)
import Tensor (Bilinear(..), Vec(Vec))
import Data.Maybe (catMaybes)
import Data.Constraint (Dict(Dict))
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..), sumV)

class Monoid (VecBuilder v) => BasicVector v where
    type VecBuilder v :: Type
    sumBuilder :: [VecBuilder v] -> v

newtype NumBuilder a = NumBuilder { unNumBuilder :: a }

instance Num a => Semigroup (NumBuilder a) where
    NumBuilder x <> NumBuilder y = NumBuilder (x+y)

instance Num a => Monoid (NumBuilder a) where
    mempty = NumBuilder 0

instance BasicVector Float where
    type VecBuilder Float = NumBuilder Float
    sumBuilder = sum . fmap unNumBuilder

instance BasicVector Double where
    type VecBuilder Double = NumBuilder Double
    sumBuilder = sum . fmap unNumBuilder

class BasicVector v => ProdVector v where
    zeroBuilder :: VecBuilder v
    identityBuilder :: v -> VecBuilder v

instance ProdVector Float where
    zeroBuilder = NumBuilder 0
    identityBuilder = NumBuilder

instance ProdVector Double where
    zeroBuilder = NumBuilder 0
    identityBuilder = NumBuilder

class ProdVector v => FullVector v where
    negateBuilder :: v -> VecBuilder v
    scaleBuilder :: Scalar v -> v -> VecBuilder v

newtype Dense a = Dense a
    deriving AdditiveGroup via a
    deriving VectorSpace via a

newtype VSpaceBuilder a = VSpaceBuilder { unVSpaceBuilder :: a }

instance AdditiveGroup a => Semigroup (VSpaceBuilder a) where
    VSpaceBuilder x <> VSpaceBuilder y = VSpaceBuilder (x ^+^ y)

instance AdditiveGroup a => Monoid (VSpaceBuilder a) where
    mempty = VSpaceBuilder zeroV

instance AdditiveGroup a => BasicVector (Dense a) where
    type VecBuilder (Dense a) = VSpaceBuilder a
    sumBuilder = Dense . sumV . fmap unVSpaceBuilder

instance AdditiveGroup a => ProdVector (Dense a) where
    zeroBuilder = VSpaceBuilder zeroV
    identityBuilder (Dense x) = VSpaceBuilder x
instance VectorSpace a => FullVector (Dense a) where
    negateBuilder (Dense a) = VSpaceBuilder (negateV a)
    scaleBuilder a (Dense v) = VSpaceBuilder (a *^ v)

instance FullVector Float where
    negateBuilder = NumBuilder . negate
    scaleBuilder x = NumBuilder . (x*)
instance FullVector Double where
    negateBuilder = NumBuilder . negate
    scaleBuilder x = NumBuilder . (x*)
    
instance (BasicVector a, BasicVector b) => BasicVector (a, b) where
    type VecBuilder (a, b) = (Maybe (VecBuilder a), Maybe (VecBuilder b))
    sumBuilder xs =
        ( sumBuilder (catMaybes (fst <$> xs))
        , sumBuilder (catMaybes (snd <$> xs))
        )

instance (ProdVector a, ProdVector b) => ProdVector (a, b) where
    zeroBuilder = (Nothing, Nothing)
    identityBuilder (x, y) = (Just (identityBuilder x), Just (identityBuilder y))

fstF1 :: ProdVector du => BackFunc (du, dv) du
fstF1 = BackFunc back
    where back x = (Just (identityBuilder x), Nothing)

intoFst :: ProdVector du => BackFunc du (du, dv)
intoFst = BackFunc fwd
    where fwd (x, _) = identityBuilder x


sndF1 :: ProdVector dv => BackFunc (du, dv) dv
sndF1 = BackFunc back
    where back x = (Nothing, Just (identityBuilder x))

intoSnd :: ProdVector dv => BackFunc dv (du, dv)
intoSnd = BackFunc fwd
    where fwd (_, x) = identityBuilder x

type BasicVectors v dv = (BasicVector v, BasicVector dv) -- TODO: remove?
type FullVectors v dv = (FullVector v, FullVector dv, Scalar v ~ Scalar dv)

data BackFunc u v = BackFunc (v -> VecBuilder u)
data FwdFunc u v = FwdFunc (u -> VecBuilder v)

newtype Vec' dx x = Vec' { unVec' :: x }
    deriving Show
    deriving AdditiveGroup via x

newtype Covec' dx x = Covec' { uncovec' :: dx }
    deriving Show
    deriving AdditiveGroup via dx

instance Bilinear (FwdFunc dv du) (Vec dv) where
    type FwdFunc dv du ✕ Vec dv = VecBuilder du
    FwdFunc f ✕ Vec dv = f dv


class Transpose (f :: Type -> Type -> Type) (g :: Type -> Type -> Type) | f->g, g->f where
    transpose :: forall u v. f u v -> g v u
    flipTranspose :: Dict (Transpose g f)

instance Transpose BackFunc FwdFunc where
    transpose (BackFunc f) = FwdFunc f
    flipTranspose = Dict
instance Transpose FwdFunc BackFunc where
    transpose (FwdFunc f) = BackFunc f
    flipTranspose = Dict

flipFunc1 :: BackFunc du dv -> FwdFunc dv du
flipFunc1 (BackFunc f) = FwdFunc f

class LinearEdge e where
    negateFunc :: FullVector du => e du du
    scaleFunc :: FullVector du => Scalar du -> e du du
    identityFunc :: FullVector du => e du du

instance LinearEdge BackFunc where
    scaleFunc a = BackFunc (scaleBuilder a)
    negateFunc = BackFunc negateBuilder
    identityFunc = BackFunc identityBuilder

instance BasicVector Integer where
    type VecBuilder Integer = NumBuilder Integer
    sumBuilder = Prelude.sum . map unNumBuilder

instance ProdVector Integer where
    zeroBuilder = NumBuilder 0
    identityBuilder = NumBuilder

instance FullVector Integer where
    negateBuilder = NumBuilder . Prelude.negate
    scaleBuilder a = NumBuilder . (a *)