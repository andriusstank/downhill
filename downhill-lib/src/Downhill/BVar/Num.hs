{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.BVar.Num
  ( -- | Automatic differentiation for @Num@ hierarchy.
    --
    -- Polymorphic functions of type such as @Num a => a -> a@
    -- can't be differentiated directly, because 'backprop' needs some additional instances.
    -- 'AsNum' wrapper provides those instances.
    --
    -- @
    -- derivative :: (forall b. Floating b => b -> b) -> (forall a. Floating a => a -> a)
    -- derivative fun x0 = backpropNum (fun (var (AsNum x0)))
    -- @

    AsNum (..),
    NumBVar,
    numbvarValue,
    var,
    constant,
    backpropNum
  )
where

import Data.AffineSpace (AffineSpace (..))
import Data.Semigroup (Sum (Sum, getSum))
import Data.VectorSpace (AdditiveGroup (..), VectorSpace (..), zeroV)
import Downhill.BVar (BVar (bvarValue), backprop)
import qualified Downhill.BVar as BVar
import Downhill.Grad
  ( Dual (evalGrad),
    HasGrad (Grad, Metric, MScalar, Tang),
    MetricTensor (MtCovector, MtVector, evalMetric),
  )
import Downhill.Linear.Expr (BasicVector (..), FullVector (identityBuilder, negateBuilder, scaleBuilder))

-- | @AsNum a@ implements many instances in terms of @Num a@ instance.
newtype AsNum a = AsNum {unAsNum :: a}
  deriving (Show)
  deriving (Num) via a
  deriving (Fractional) via a
  deriving (Floating) via a

instance Num a => Dual (AsNum a) (AsNum a) (AsNum a) where
  evalGrad = (*)

instance Num a => HasGrad (AsNum a) where
  type MScalar (AsNum a) = AsNum a
  type Grad (AsNum a) = AsNum a
  type Tang (AsNum a) = AsNum a
  type Metric (AsNum a) = AsNum a

instance Num a => MetricTensor (AsNum a) where
  type MtVector (AsNum a) = AsNum a
  type MtCovector (AsNum a) = AsNum a
  evalMetric (AsNum m) (AsNum x) = AsNum (m * x)

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
  AsNum x .-. AsNum y = AsNum (x - y)
  AsNum x .+^ AsNum y = AsNum (x + y)

type NumBVar a = BVar (AsNum a) (AsNum a)

constant :: forall a. Num a => a -> NumBVar a
constant = BVar.constant @(AsNum a) @(AsNum a) . AsNum

var :: Num a => a -> NumBVar a
var = BVar.var . AsNum

backpropNum :: forall a. Num a => NumBVar a -> a
backpropNum x = unAsNum $ backprop @(AsNum a) @(AsNum a) x (AsNum 1)

numbvarValue :: NumBVar a -> a
numbvarValue = unAsNum . bvarValue
