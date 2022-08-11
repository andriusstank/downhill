{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import GHC.Generics (Generic)
import Downhill.Linear.Expr (BasicVector(..))
import Data.Monoid.Generic (GenericSemigroup (..))
import Downhill.Grad (Dual, Manifold(..))
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))

data MyRecord = MyRecord {
  fieldA :: Double,
  fieldB :: (Double, Double)
} deriving Generic

data MyRecordBuilder = MyRecordBuilder {
  fieldA :: VecBuilder Double,
  fieldB :: VecBuilder (Double, Double)
} deriving Generic

instance AdditiveGroup MyRecord
instance VectorSpace MyRecord where
  type Scalar MyRecord = Double

deriving via (GenericSemigroup MyRecordBuilder) instance (Semigroup MyRecordBuilder)

instance Dual MyRecord MyRecord where

instance BasicVector MyRecord where
  type VecBuilder MyRecord = Maybe MyRecordBuilder

instance Manifold MyRecord where
  type Tang MyRecord = MyRecord
  type Grad MyRecord = MyRecord

main :: IO ()
main = return ()