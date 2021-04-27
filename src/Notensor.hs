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
, BackFun(..), FwdFun(..), flipFunc1, ProdVector(..)
, LinearEdge(..)
) where
import Data.Kind (Type)
import Data.Maybe (catMaybes)
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

newtype BackFun u v = BackFun { unBackFun :: v -> VecBuilder u }
newtype FwdFun u v = FwdFun  {unFwdFun :: u -> VecBuilder v }

flipFunc1 :: BackFun du dv -> FwdFun dv du
flipFunc1 (BackFun f) = FwdFun f

class LinearEdge e where
    negateFunc :: FullVector du => e du du
    scaleFunc :: FullVector du => Scalar du -> e du du
    identityFunc :: FullVector du => e du du

instance LinearEdge BackFun where
    scaleFunc a = BackFun (scaleBuilder a)
    negateFunc = BackFun negateBuilder
    identityFunc = BackFun identityBuilder
