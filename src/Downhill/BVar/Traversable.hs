{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}

module Downhill.BVar.Traversable
  ( TraversableVar (..),
    IntmapVector (..),
    splitTraversable,
    backpropTraversable,
  )
where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.AdditiveGroup (AdditiveGroup, sumV)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.VectorSpace (AdditiveGroup (negateV, zeroV, (^+^), (^-^)), VectorSpace ((*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.DVar (BVar (BVar), backprop, var)
import Downhill.Grad (Dual (evalGrad), HasFullGrad, HasGrad (Tang, Grad, Scalar))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Downhill.Linear.Lift (lift1_sparse)

-- | 'Traversable' types can be provided 'HasGrad' instance by deriving via @TraversableVar@.
--
-- @
-- data MyRecord a = ...
-- deriving via (TraversableVar MyRecord a) instance HasGrad a => HasGrad (MyRecord a)
-- @
newtype TraversableVar f a = TraversableVar { unTraversableVar :: f a }
    deriving stock (Functor, Foldable, Traversable)

instance HasGrad a => HasGrad (TraversableVar f a) where
  type Scalar (TraversableVar f a) = Scalar a
  type Tang (TraversableVar f a) = IntmapVector (Tang a)
  type Grad (TraversableVar f a) = IntmapVector (Grad a)

-- | @IntmapVector@ serves as a gradient of 'TraversableVar'.

newtype IntmapVector v = IntmapVector {unIntmapVector :: IntMap v}
  deriving (Show)

instance AdditiveGroup a => AdditiveGroup (IntmapVector a) where
  zeroV = IntmapVector IntMap.empty
  negateV (IntmapVector v) = IntmapVector (negateV <$> v)
  IntmapVector u ^+^ IntmapVector v = IntmapVector (IntMap.unionWith (^+^) u v)
  IntmapVector u ^-^ IntmapVector v = IntmapVector (IntMap.mergeWithKey combine only1 only2 u v)
    where combine _key x y = Just (x ^-^ y)
          only1 = id
          only2 = fmap negateV

instance VectorSpace v => VectorSpace (IntmapVector v) where
  type Scalar (IntmapVector v) = VectorSpace.Scalar v
  a *^ (IntmapVector v) = IntmapVector (fmap (a *^) v)

instance Dual s dv v => Dual s (IntmapVector dv) (IntmapVector v) where
  evalGrad (IntmapVector dv) (IntmapVector v) = sumV $ IntMap.intersectionWith evalGrad dv v

deriving via (IntMap v) instance Semigroup v => Semigroup (IntmapVector v)

deriving via (IntMap v) instance Monoid v => Monoid (IntmapVector v)

instance BasicVector v => BasicVector (IntmapVector v) where
  type VecBuilder (IntmapVector v) = IntmapVector (VecBuilder v)
  sumBuilder (IntmapVector v) = IntmapVector (fmap sumBuilder v)

imap ::
  forall t a b.
  Traversable t =>
  (Int -> a -> b) ->
  t a ->
  t b
imap mkBVar' xs' = evalState (traverse getmkvar xs') 0
  where
    getmkvar :: a -> State Int b
    getmkvar x = do
      index <- get
      put (index + 1)
      return (mkBVar' index x)

splitTraversable ::
  forall f r a.
  ( Traversable f,
    Grad (f a) ~ IntmapVector (Grad a),
    HasGrad a
  ) =>
  BVar r (f a) ->
  f (BVar r a)
splitTraversable (BVar xs dxs) = vars
  where
    vars :: f (BVar r a)
    vars = imap mkBVar xs
    mkBVar :: Int -> a -> BVar r a
    mkBVar index x =
      let mkBuilder :: VecBuilder (Grad a) -> IntmapVector (VecBuilder (Grad a))
          mkBuilder dx = IntmapVector (IntMap.singleton index dx)
       in BVar x (lift1_sparse mkBuilder dxs)

backpropTraversable ::
  forall f a b p.
  ( Traversable f,
    Grad (f a) ~ IntmapVector (Grad a),
    HasFullGrad a,
    HasFullGrad p
  ) =>
  Grad p ->
  (a -> Grad a -> b) ->
  (forall r. f (BVar r a) -> BVar r p) ->
  f a ->
  f b
backpropTraversable one combine fun x = imap makeResult x
  where
    splitX :: f (BVar (IntmapVector (Grad a)) a)
    splitX = unTraversableVar (splitTraversable (var (TraversableVar x)))

    y :: BVar (IntmapVector (Grad a)) p
    y = fun splitX

    grad :: IntMap (Grad a)
    IntmapVector grad = backprop y one

    lookupGrad i = fromMaybe zeroV (IntMap.lookup i grad)

    makeResult :: Int -> a -> b
    makeResult i x' = combine x' (lookupGrad i)
