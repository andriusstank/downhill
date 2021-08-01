{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.VectorSpace (AdditiveGroup ((^+^), zeroV), VectorSpace)
import qualified Data.VectorSpace as VectorSpace
import Downhill.Grad (Dual (evalGrad), HasGrad (Diff, Grad, Scalar))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder), DenseBuilder (DenseBuilder), maybeToMonoid, FullVector (identityBuilder, negateBuilder, scaleBuilder))
import GHC.Generics (Generic)
import Downhill.DVar (BVar(BVar))
import Downhill.Linear.Lift (lift1_sparse, lift2_sparse)
import Downhill.Linear.BackGrad

data MyRecord a b = MyRecord
  { fieldA :: a,
    fieldB :: b
  }
  deriving (Show, Generic)

instance (Semigroup a, Semigroup b) => Semigroup (MyRecord a b)
    where (MyRecord a1 b1) <> (MyRecord a2 b2) = MyRecord (a1<>a2) (b1<>b2)

instance (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (MyRecord a b)

instance (VectorSpace a, VectorSpace b, VectorSpace.Scalar a ~ VectorSpace.Scalar b) => VectorSpace (MyRecord a b)

instance (BasicVector a, BasicVector b) => BasicVector (MyRecord a b) where
  type VecBuilder (MyRecord a b) = Maybe (MyRecord (VecBuilder a) (VecBuilder b))
  sumBuilder r = MyRecord (sumBuilder a) (sumBuilder b)
    where a = maybeToMonoid (fieldA <$> r)
          b = maybeToMonoid (fieldB <$> r)

instance (Dual s da a, Dual s db b) => Dual s (MyRecord da db) (MyRecord a b) where
  evalGrad (MyRecord da db) (MyRecord a b) = evalGrad da a ^+^ evalGrad db b

instance (FullVector a, FullVector b, VectorSpace.Scalar a ~ VectorSpace.Scalar b) => FullVector (MyRecord a b) where
    identityBuilder (MyRecord a b) = Just (MyRecord (identityBuilder a) (identityBuilder b))
    negateBuilder (MyRecord a b) = Just (MyRecord (negateBuilder a) (negateBuilder b))
    scaleBuilder x (MyRecord a b) = Just (MyRecord (scaleBuilder x a) (scaleBuilder x b))

instance (HasGrad a, HasGrad b, Scalar a ~ Scalar b) => HasGrad (MyRecord a b) where
  type Scalar (MyRecord a b) = Scalar a
  type Diff (MyRecord a b) = MyRecord (Diff a) (Diff b)
  type Grad (MyRecord a b) = MyRecord (Grad a) (Grad b)


{-# ANN splitRecord "HLint: Avoid lambda using `infix`" #-}
splitRecord :: forall r a b. (HasGrad a, HasGrad b) => BVar r (MyRecord a b) -> MyRecord (BVar r a) (BVar r b)
splitRecord (BVar x dx) = MyRecord (BVar (fieldA x) da) (BVar (fieldB x) db)
    where da :: BackGrad r (Grad a)
          da = lift1_sparse (\dx_da -> Just (MyRecord dx_da mempty)) dx
          db :: BackGrad r (Grad b)
          db = lift1_sparse (\dx_db -> Just (MyRecord mempty dx_db)) dx

splitRecord2 :: forall r a b. (HasGrad a, HasGrad b) => BVar r (MyRecord a b) -> MyRecord (BVar r a) (BVar r b)
splitRecord2 (BVar x dx) = MyRecord (BVar (fieldA x) da) (BVar (fieldB x) db)
    where da :: BackGrad r (Grad a)
          da = lift1_sparse (\dx_da -> Just (MyRecord dx_da mempty)) dx
          db :: BackGrad r (Grad b)
          db = lift1_sparse (\dx_db -> Just (MyRecord mempty dx_db)) dx
          da' :: a1 -> Maybe (MyRecord a1 b0)
          da' dx_da = Just (MyRecord dx_da mempty)

joinRecord :: forall r a b. (HasGrad a, HasGrad b) => MyRecord (BVar r a) (BVar r b) -> BVar r (MyRecord a b)
joinRecord (MyRecord (BVar a da) (BVar b db)) = BVar (MyRecord a b) (lift2_sparse bpA bpB da db)
    where bpA :: Maybe (MyRecord (VecBuilder (Grad a)) (VecBuilder (Grad b))) -> VecBuilder (Grad a)
          bpA = maybeToMonoid . fmap fieldA
          bpB :: Maybe (MyRecord (VecBuilder (Grad a)) (VecBuilder (Grad b))) -> VecBuilder (Grad b)
          bpB = maybeToMonoid . fmap fieldB


main :: IO ()
main = return ()
