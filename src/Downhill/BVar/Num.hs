{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.BVar.Num
  ( -- | Automatic differentiation for @Num@ hierarchy.
    AsNum (..), NumBVar,
    numbvarValue,
    var,
    constant,
    backpropNum,
  )
where

import Data.AffineSpace (AffineSpace (..))
import Data.Semigroup (Sum (Sum, getSum))
import Data.VectorSpace (AdditiveGroup (..), VectorSpace (..), zeroV)
import Downhill.DVar (BVar, backprop, DVar (dvarValue))
import qualified Downhill.DVar as DVar
import Downhill.Linear.BackGrad (HasGrad (..))
import Downhill.Linear.Expr (BasicVector (..), FullVector (identityBuilder, negateBuilder, scaleBuilder))

-- | Use @Num a@ instance to provide @VectorSpace@ and its friends.
newtype AsNum a = AsNum {unAsNum :: a}
  deriving (Show)
  deriving (Num) via a
  deriving (Fractional) via a
  deriving (Floating) via a

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
  AsNum x .-. AsNum y = AsNum (x - y)
  AsNum x .+^ AsNum y = AsNum (x + y)

type NumBVar a = BVar (AsNum a) (AsNum a)

constant :: Num a => a -> NumBVar a
constant = DVar.constant . AsNum

var :: Num a => a -> NumBVar a
var = DVar.var . AsNum

backpropNum :: forall a. Num a => NumBVar a -> a
backpropNum x = unAsNum $ backprop @(AsNum a) @(AsNum a) x (AsNum 1)

numbvarValue :: NumBVar a -> a
numbvarValue = unAsNum . dvarValue

