{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Easy backpropagation when all variables have the same type.
--
-- @
-- data MyRecord a = ...
--   deriving (Functor, Foldable, Traversable)
--
-- deriving via (TraversableVar MyRecord a) instance HasGrad a => HasGrad (MyRecord a)
-- @
--
-- = Gradient type
-- One might excect gradient type to be @type Grad (MyRecord a) = MyRecord (Grad a)@, but it's not
-- the case, because record could contain additional members apart from @a@s, for example:
--
-- @
-- data MyPoint a = MyPoint
-- {
-- ,  pointLabel :: String
-- ,  pointX :: a
-- ,  pointY :: a
-- }
-- @
--
-- and @MyPoint (Grad a)@ can't be made @VectorSpace@. Gradient type @Grad (MyRecord a)@
-- is a newtype wrapper over @IntMap@
-- that is not exported.
module Downhill.BVar.Traversable
  ( -- * Backpropagate
    backpropTraversable,
    backpropTraversable_GradOnly,
    backpropTraversable_ValueAndGrad,

    -- * Split
    splitTraversable,

    -- * TraversableVar
    TraversableVar (..),
  )
where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.AdditiveGroup (AdditiveGroup, sumV)
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.VectorSpace (AdditiveGroup (negateV, zeroV, (^+^), (^-^)), VectorSpace (Scalar, (*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.BVar (BVar (BVar, bvarGrad, bvarValue), backprop, var)
import Downhill.Grad
  ( Dual (evalGrad),
    HasGrad (Grad, MScalar, Metric, Tang),
    MetricTensor
      ( MtCovector,
        MtVector,
        evalMetric
      ),
  )
import Downhill.Linear.BackGrad (BackGrad (BackGrad), castBackGrad, realNode)
import Downhill.Linear.Expr
  ( BasicVector (VecBuilder, sumBuilder, identityBuilder),
    Expr (ExprSum),
    SparseVector (unSparseVector),
    Term,
  )
import Downhill.Linear.Lift (lift1_sparse)
import GHC.Generics (Generic)

-- | Provides HasGrad instance for use in deriving via
newtype TraversableVar f a = TraversableVar {unTraversableVar :: f a}
  deriving stock (Functor, Foldable, Traversable)

newtype TraversableMetric f a = TraversableMetric (Metric a)
  deriving (Generic)

instance AdditiveGroup (Metric a) => AdditiveGroup (TraversableMetric f a)

instance VectorSpace (Metric a) => VectorSpace (TraversableMetric f a) where
  type Scalar (TraversableMetric f a) = Scalar (Metric a)

instance
  ( MetricTensor s (Metric a),
    MtVector (Metric a) ~ Tang a,
    MtCovector (Metric a) ~ Grad a,
    Dual s (Tang a) (Grad a)
  ) =>
  MetricTensor s (TraversableMetric f a)
  where
  type MtVector (TraversableMetric f a) = IntmapVector f (Tang a)
  type MtCovector (TraversableMetric f a) = IntmapVector f (Grad a)
  evalMetric (TraversableMetric m) (IntmapVector da) = IntmapVector (IntMap.map (evalMetric m) da)

instance HasGrad a => HasGrad (TraversableVar f a) where
  type MScalar (TraversableVar f a) = MScalar a
  type Tang (TraversableVar f a) = IntmapVector f (Tang a)
  type Grad (TraversableVar f a) = IntmapVector f (Grad a)
  type Metric (TraversableVar f a) = TraversableMetric f a

-- | @IntmapVector@ serves as a gradient of 'TraversableVar'.
newtype IntmapVector f v = IntmapVector {unIntmapVector :: IntMap v}
  deriving (Show)

instance AdditiveGroup a => AdditiveGroup (IntmapVector f a) where
  zeroV = IntmapVector IntMap.empty
  negateV (IntmapVector v) = IntmapVector (negateV <$> v)
  IntmapVector u ^+^ IntmapVector v = IntmapVector (IntMap.unionWith (^+^) u v)
  IntmapVector u ^-^ IntmapVector v = IntmapVector (IntMap.mergeWithKey combine only1 only2 u v)
    where
      combine _key x y = Just (x ^-^ y)
      only1 = id
      only2 = fmap negateV

instance VectorSpace v => VectorSpace (IntmapVector f v) where
  type Scalar (IntmapVector f v) = VectorSpace.Scalar v
  a *^ (IntmapVector v) = IntmapVector (fmap (a *^) v)

instance Dual s dv v => Dual s (IntmapVector f dv) (IntmapVector f v) where
  evalGrad (IntmapVector dv) (IntmapVector v) = sumV $ IntMap.intersectionWith evalGrad dv v

deriving via (IntMap v) instance Semigroup v => Semigroup (IntmapVector f v)

deriving via (IntMap v) instance Monoid v => Monoid (IntmapVector f v)

instance BasicVector v => BasicVector (IntmapVector f v) where
  type VecBuilder (IntmapVector f v) = IntmapVector f (VecBuilder v)
  sumBuilder (IntmapVector v) = IntmapVector (fmap sumBuilder v)
  identityBuilder (IntmapVector x) = IntmapVector (identityBuilder <$> x)

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

-- | Note that @splitTraversable@ won't be useful
-- for top level @BVar@, because the type @Grad (f a)@ is not exposed.
splitTraversable ::
  forall f r a.
  ( Traversable f,
    Grad (f a) ~ Grad (TraversableVar f a),
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
      let mkBuilder :: VecBuilder (Grad a) -> IntmapVector f (VecBuilder (Grad a))
          mkBuilder dx = IntmapVector (IntMap.singleton index dx)
       in BVar x (lift1_sparse mkBuilder dxs)

lift1_sparseT ::
  forall r a z.
  BasicVector z =>
  (VecBuilder z -> VecBuilder a) ->
  BackGrad r a ->
  Term r (SparseVector z)
lift1_sparseT fa (BackGrad f) = f (fa . unSparseVector)

-- Not exported, because it is untested and hardly useful.
_joinTraversable ::
  forall f r a.
  ( Traversable f,
    Grad (f a) ~ Grad (TraversableVar f a),
    HasGrad a
  ) =>
  f (BVar r a) ->
  BVar r (f a)
_joinTraversable x = BVar values (castBackGrad node)
  where
    values :: f a
    values = bvarValue <$> x
    grads :: f (BackGrad r (Grad a))
    grads = bvarGrad <$> x
    terms :: [Term r (SparseVector (IntmapVector f (Grad a)))]
    terms = toList (imap mkTerm grads)
    mkTerm :: Int -> BackGrad r (Grad a) -> Term r (SparseVector (IntmapVector f (Grad a)))
    mkTerm index = lift1_sparseT (lookupIntMap index)
    lookupIntMap :: Int -> IntmapVector f x -> x
    lookupIntMap key (IntmapVector intmap) = case IntMap.lookup key intmap of
      Nothing -> error "Downhill BUG: Bad index in joinTraversable"
      Just value -> value
    node :: BackGrad r (SparseVector (IntmapVector f (Grad a)))
    node = realNode (ExprSum terms)

-- | @backpropTraversable one combine fun@
--
-- @one@ is a value to be backpropagated. In case of @p@ being scalar, set @one@
-- to 1 to compute unscaled gradient.
--
-- @combine@ is given value of a parameter and its gradient to construct result,
-- just like @zipWith@.
--
-- @fun@ is the function to be differentiated.
backpropTraversable ::
  forall f a b p.
  ( Traversable f,
    Grad (f a) ~ Grad (TraversableVar f a),
    HasGrad a,
    HasGrad p
  ) =>
  Grad p ->
  (a -> Grad a -> b) ->
  (forall r. f (BVar r a) -> BVar r p) ->
  f a ->
  f b
backpropTraversable one combine fun x = imap makeResult x
  where
    splitX :: f (BVar (Grad (f a)) a)
    splitX = splitTraversable (var x)

    y :: BVar (Grad (f a)) p
    y = fun splitX

    grad :: IntMap (Grad a)
    IntmapVector grad = backprop y one

    lookupGrad i = fromMaybe zeroV (IntMap.lookup i grad)

    makeResult :: Int -> a -> b
    makeResult i x' = combine x' (lookupGrad i)

{-# ANN backpropTraversable_GradOnly "HLint: ignore Use camelCase" #-}

-- | Like 'backpropTraversable', but returns gradient only.
backpropTraversable_GradOnly ::
  forall f a p.
  ( Traversable f,
    Grad (f a) ~ Grad (TraversableVar f a),
    HasGrad a,
    HasGrad p
  ) =>
  Grad p ->
  (forall r. f (BVar r a) -> BVar r p) ->
  f a ->
  f (Grad a)
backpropTraversable_GradOnly one = backpropTraversable one gradOnly
  where
    gradOnly _value grad = grad

-- | 'backpropTraversable' specialized to return a pair of value and gradient.
{-# ANN backpropTraversable_ValueAndGrad "HLint: ignore Use camelCase" #-}
backpropTraversable_ValueAndGrad ::
  forall f a p.
  ( Traversable f,
    Grad (f a) ~ Grad (TraversableVar f a),
    HasGrad a,
    HasGrad p
  ) =>
  Grad p ->
  (forall r. f (BVar r a) -> BVar r p) ->
  f a ->
  f (a, Grad a)
backpropTraversable_ValueAndGrad one = backpropTraversable one (,)
