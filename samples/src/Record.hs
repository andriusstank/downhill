{- Manually accessing members of a record. Lot's of boilerplate, which
can be generated with Template Haskell from Downhill. This sample demonstrates
how it's done.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main (main, splitRecord, joinRecord) where

import Data.VectorSpace (AdditiveGroup ((^+^)), VectorSpace (Scalar))
import qualified Data.VectorSpace as VectorSpace
import Downhill.BVar (BVar (BVar))
import Downhill.Grad
  ( Dual (evalGrad),
    GradBuilder,
    Manifold (Grad, Tang), MScalar
  )
import Downhill.Linear.Expr (BasicVector (VecBuilder, identityBuilder, sumBuilder), maybeToMonoid)
import Downhill.Linear.Lift (lift1_sparse, lift2_sparse)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Downhill.Metric (MetricTensor (evalMetric, sqrNorm))

data MyRecord a b = MyRecord
  { fieldA :: a,
    fieldB :: b
  }
  deriving (Show, Generic)

instance (Semigroup a, Semigroup b) => Semigroup (MyRecord a b) where
  (MyRecord a1 b1) <> (MyRecord a2 b2) = MyRecord (a1 <> a2) (b1 <> b2)

instance (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (MyRecord a b)

instance (VectorSpace a, VectorSpace b, VectorSpace.Scalar a ~ VectorSpace.Scalar b) => VectorSpace (MyRecord a b)

instance (BasicVector a, BasicVector b) => BasicVector (MyRecord a b) where
  type VecBuilder (MyRecord a b) = Maybe (MyRecord (VecBuilder a) (VecBuilder b))
  sumBuilder r = MyRecord (sumBuilder a) (sumBuilder b)
    where
      a = maybeToMonoid (fieldA <$> r)
      b = maybeToMonoid (fieldB <$> r)
  identityBuilder (MyRecord a b) = Just (MyRecord (identityBuilder a) (identityBuilder b))

instance (Scalar a ~ Scalar b, Dual da a, Dual db b) => Dual (MyRecord da db) (MyRecord a b) where
  evalGrad (MyRecord da db) (MyRecord a b) = evalGrad da a ^+^ evalGrad db b

instance
  ( MScalar a ~ MScalar b,
    MetricTensor a ga,
    MetricTensor b gb
  ) =>
  MetricTensor (MyRecord a b) (MyRecord ga gb)
  where
  evalMetric (MyRecord m1 m2) (MyRecord x1 x2) = MyRecord (evalMetric @a m1 x1) (evalMetric @b m2 x2)
  sqrNorm (MyRecord m1 m2) (MyRecord x1 x2) = sqrNorm @a m1 x1 ^+^ sqrNorm @b m2 x2

instance (Manifold a, Manifold b, MScalar a ~ MScalar b) => Manifold (MyRecord a b) where
  type Tang (MyRecord a b) = MyRecord (Tang a) (Tang b)
  type Grad (MyRecord a b) = MyRecord (Grad a) (Grad b)

splitRecord :: forall r a b. (BasicVector (Grad a), BasicVector (Grad b)) => BVar r (MyRecord a b) -> MyRecord (BVar r a) (BVar r b)
splitRecord x = MyRecord (getField @"fieldA" x) (getField @"fieldB" x)

joinRecord :: forall r a b. (BasicVector (Grad a), BasicVector (Grad b)) => MyRecord (BVar r a) (BVar r b) -> BVar r (MyRecord a b)
joinRecord (MyRecord (BVar a da) (BVar b db)) = BVar (MyRecord a b) (lift2_sparse bpA bpB da db)
  where
    bpA :: Maybe (MyRecord (VecBuilder (Grad a)) (VecBuilder (Grad b))) -> VecBuilder (Grad a)
    bpA = maybeToMonoid . fmap fieldA
    bpB :: Maybe (MyRecord (VecBuilder (Grad a)) (VecBuilder (Grad b))) -> VecBuilder (Grad b)
    bpB = maybeToMonoid . fmap fieldB

instance (BasicVector (Grad a), BasicVector (Grad b)) => HasField "fieldA" (BVar r (MyRecord a b)) (BVar r a) where
  getField :: BVar r (MyRecord a b) -> BVar r a
  getField (BVar x dx) = BVar (fieldA x) (lift1_sparse go dx)
    where
      go :: GradBuilder a -> Maybe (MyRecord (GradBuilder a) (GradBuilder b))
      go dx_da = Just (MyRecord dx_da mempty)

instance (BasicVector (Grad a), BasicVector (Grad b)) => HasField "fieldB" (BVar r (MyRecord a b)) (BVar r b) where
  getField :: BVar r (MyRecord a b) -> BVar r b
  getField (BVar x dx) = BVar (fieldB x) (lift1_sparse go dx)
    where
      go :: GradBuilder b -> Maybe (MyRecord (GradBuilder a) (GradBuilder b))
      go dx_db = Just (MyRecord mempty dx_db)

main :: IO ()
main = return ()
