{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BVar.Num
where
import Affine (DVar(DVar))
import Data.Kind (Type)
import Data.VectorSpace (zeroV, AdditiveGroup(..), VectorSpace(..))
import Data.AffineSpace (AffineSpace(..))
import Downhill.Linear.Expr (Expr, Expr (ExprVar), BasicVector(..))
import Notensor (FullVector(..))
import EType (Endpoint(SourceNode))
import Diff (backprop, HasGrad(..), BVar)
import qualified Diff
import Data.Semigroup (Sum(Sum, getSum))
import Downhill.Linear.BackGrad (HasGrad(evalGrad))

newtype AsNum a = AsNum { unAsNum :: a }
    deriving Show
    deriving Num via a
    deriving Fractional via a
    deriving Floating via a

instance Num a => HasGrad (AsNum a) where
    type GradOf (AsNum a) = AsNum a
    evalGrad = (*)

instance Num a => AdditiveGroup (AsNum a) where
    zeroV = 0
    (^+^) = (+)
    (^-^) = (-)
    negateV = negate

instance Num a => VectorSpace (AsNum a) where
    type Scalar (AsNum a) = AsNum a
    (*^) = (*)

instance Num a => BasicVector (AsNum a) where
    type VecBuilder (AsNum a) = Sum a
    sumBuilder = AsNum . getSum

instance Num a => FullVector (AsNum a) where
    identityBuilder = Sum . unAsNum
    negateBuilder = Sum . negate . unAsNum
    scaleBuilder (AsNum x) (AsNum y) = Sum $ x * y

instance Num a => AffineSpace (AsNum a) where
    type Diff (AsNum a) = AsNum a
    AsNum x .-. AsNum y = AsNum (x-y)
    AsNum x .+^ AsNum y = AsNum (x+y)

newtype NumBVar a = NumBVar (BVar (AsNum a) (AsNum a))
    deriving (Num, Fractional, Floating)

constant :: Num a => a -> NumBVar a
constant x = NumBVar (Diff.constant (AsNum x))

var :: Num a => a -> NumBVar a
var x = NumBVar (Diff.var (AsNum x))

backpropNum :: forall a. Num a => NumBVar a -> a
backpropNum (NumBVar x) = unAsNum $ backprop @(AsNum a) @(AsNum a) x (AsNum 1)

numbvarValue :: NumBVar a -> a
numbvarValue (NumBVar (DVar y0 _dy)) = unAsNum y0
