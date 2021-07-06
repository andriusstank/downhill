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
import Downhill.DVar (BVar, backprop, DVar (dvarValue), HasGrad)
import qualified Downhill.DVar as DVar
import Downhill.Linear.BackGrad (HasDual (..))
import Downhill.Linear.Expr (BasicVector (..), FullVector (identityBuilder, negateBuilder, scaleBuilder))
import Math.Manifold.Core.PseudoAffine (Semimanifold(..), BoundarylessWitness (BoundarylessWitness), SemimanifoldWitness (SemimanifoldWitness))
import Data.Tagged (Tagged(..))

-- | Use @Num a@ instance to provide @VectorSpace@ and its friends.
newtype AsNum a = AsNum {unAsNum :: a}
  deriving (Show)
  deriving (Num) via a
  deriving (Fractional) via a
  deriving (Floating) via a

instance Num a => HasGrad (AsNum a) where
  type Grad (AsNum a) = AsNum a

instance Num a => HasDual (AsNum a) where
  type DualOf (AsNum a) = AsNum a
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

instance Num a => Semimanifold (AsNum a) where
  type Needle (AsNum a) = AsNum a
  type Interior (AsNum a) = AsNum a
  (.+~^) = (+)
  fromInterior = id
  toInterior = Just
  translateP = Tagged (+)
  (.-~^) = (-)
  semimanifoldWitness = SemimanifoldWitness BoundarylessWitness

type NumBVar a = BVar (AsNum a) (AsNum a)

constant :: forall a. Num a => a -> NumBVar a
constant = DVar.constant @(AsNum a) @(AsNum a) . AsNum

var :: Num a => a -> NumBVar a
var = DVar.var . AsNum

backpropNum :: forall a. Num a => NumBVar a -> a
backpropNum x = unAsNum $ backprop @(AsNum a) @(AsNum a) x (AsNum 1)

numbvarValue :: NumBVar a -> a
numbvarValue = unAsNum . dvarValue

