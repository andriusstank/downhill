{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import GHC.Generics (Generic)
import Downhill.Linear.Expr (BasicVector(..))
import Data.Monoid.Generic (GenericSemigroup (..))
import Data.AdditiveGroup (AdditiveGroup)

data MyRecord = MyRecord {
  fieldA :: Double,
  fieldB :: (Double, Double)
} deriving Generic

data MyRecordBuilder = MyRecordBuilder {
  fieldA :: VecBuilder Double,
  fieldB :: VecBuilder (Double, Double)
} deriving Generic

instance AdditiveGroup MyRecord
deriving via (GenericSemigroup MyRecordBuilder) instance (Semigroup MyRecordBuilder)

instance BasicVector MyRecord where
  type VecBuilder MyRecord = Maybe MyRecordBuilder


main :: IO ()
main = return ()