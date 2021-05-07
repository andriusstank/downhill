{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Notensor
( BasicVector(..), FullVector(..), Dense(..)
, NumBuilder(..)
, BackFun(..), FwdFun(..), flipBackFun
, maybeToMonoid
) where
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe)
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..), sumV)

maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

class Monoid (VecBuilder v) => BasicVector v where
    type VecBuilder v :: Type
    sumBuilder' :: VecBuilder v -> v
    sumBuilder :: [VecBuilder v] -> v

newtype NumBuilder a = NumBuilder { unNumBuilder :: a }

instance Num a => Semigroup (NumBuilder a) where
    NumBuilder x <> NumBuilder y = NumBuilder (x+y)

instance Num a => Monoid (NumBuilder a) where
    mempty = NumBuilder 0

instance BasicVector Float where
    type VecBuilder Float = NumBuilder Float
    sumBuilder = sum . fmap unNumBuilder
    sumBuilder' = unNumBuilder

instance BasicVector Double where
    type VecBuilder Double = NumBuilder Double
    sumBuilder = sum . fmap unNumBuilder
    sumBuilder' = unNumBuilder

class BasicVector v => FullVector v where
    identityBuilder :: v -> VecBuilder v
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
    sumBuilder' = Dense . unVSpaceBuilder

instance VectorSpace a => FullVector (Dense a) where
    identityBuilder (Dense x) = VSpaceBuilder x
    negateBuilder (Dense a) = VSpaceBuilder (negateV a)
    scaleBuilder a (Dense v) = VSpaceBuilder (a *^ v)

instance FullVector Float where
    identityBuilder = NumBuilder
    negateBuilder = NumBuilder . negate
    scaleBuilder x = NumBuilder . (x*)
instance FullVector Double where
    identityBuilder = NumBuilder
    negateBuilder = NumBuilder . negate
    scaleBuilder x = NumBuilder . (x*)

sumPair :: (BasicVector a, BasicVector b) => (VecBuilder a, VecBuilder b) -> (a, b)
sumPair (a, b) = (sumBuilder' a, sumBuilder' b)

instance (BasicVector a, BasicVector b) => BasicVector (a, b) where
    type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
    sumBuilder xs = case mconcat xs of
        Nothing -> (sumBuilder' mempty, sumBuilder' mempty)
        Just (as, bs) -> (sumBuilder' as, sumBuilder' bs)

    sumBuilder' = sumPair . maybeToMonoid

instance (Scalar a ~ Scalar b, FullVector a, FullVector b) => FullVector (a, b) where
    identityBuilder (x, y) = Just (identityBuilder x, identityBuilder y)
    negateBuilder (x, y) = Just (negateBuilder x, negateBuilder y)
    scaleBuilder a (x, y) = Just (scaleBuilder a x, scaleBuilder a y)

newtype BackFun u v = BackFun { unBackFun :: v -> VecBuilder u }
newtype FwdFun u v = FwdFun  {unFwdFun :: u -> VecBuilder v }

flipBackFun :: BackFun du dv -> FwdFun dv du
flipBackFun (BackFun f) = FwdFun f
