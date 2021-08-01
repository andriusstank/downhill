{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, StateT, get, put)
import Control.Monad.Trans.Writer (Writer, WriterT (WriterT), tell)
import Data.AdditiveGroup (AdditiveGroup (negateV, (^-^)), sumV)
import Data.Coerce (coerce)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Distributive (Distributive (distribute))
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Product (Product (Pair))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Data.Vector.Primitive (MVector (MVector))
import Data.VectorSpace (AdditiveGroup (zeroV, negateV, (^+^), (^-^)), Sum (Sum), VectorSpace ((*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.DVar (BVar (BVar))
import Downhill.Grad (Dual (evalGrad), HasGrad (Diff, Grad, Scalar))
import Downhill.Linear.BackGrad (BackGrad, realNode)
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder), DenseBuilder (DenseBuilder), FullVector (identityBuilder, negateBuilder, scaleBuilder), maybeToMonoid)
import Downhill.Linear.Lift (lift1_sparse, lift2_sparse)
import qualified Downhill.Linear.Lift as Linear
import GHC.Generics (Generic)
import qualified GHC.Real as MVector
import GHC.TypeNats (KnownNat, Nat, natVal)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Lazy as IntMap

data MyRecord a = MyRecord
  { memberPair :: (a, a),
    memberList :: [a]
  }
  deriving (Functor, Foldable, Traversable, Show)

newtype VarTracker a = VarTracker {unVarTracker :: State Int a}
  deriving (Functor, Applicative, Monad) via (State Int)

newtype TraverseVector (f :: Type -> Type) v = TraverseVector (IntMap v)

instance AdditiveGroup a => AdditiveGroup (TraverseVector f a) where
  zeroV = TraverseVector IntMap.empty
  negateV (TraverseVector v) = TraverseVector (negateV <$> v)
  TraverseVector u ^+^ TraverseVector v = TraverseVector (IntMap.unionWith (^+^) u v)
  TraverseVector u ^-^ TraverseVector v = TraverseVector (IntMap.unionWith (^-^) u v)

instance VectorSpace v => VectorSpace (TraverseVector f v) where
  type Scalar (TraverseVector f v) = VectorSpace.Scalar v
  a *^ (TraverseVector v) = TraverseVector (fmap (a *^ ) v)

instance Dual s dv v => Dual s (TraverseVector f dv) (TraverseVector f v) where
  evalGrad (TraverseVector dv) (TraverseVector v) = sumV $ IntMap.intersectionWith evalGrad dv v

deriving via (IntMap v) instance Semigroup v => Semigroup (TraverseVector f v)
deriving via (IntMap v) instance Monoid v => Monoid (TraverseVector f v)

instance BasicVector v => BasicVector (TraverseVector f v) where
  type VecBuilder (TraverseVector f v) = TraverseVector f (VecBuilder v)
  sumBuilder (TraverseVector v) = TraverseVector (fmap sumBuilder v)

instance FullVector v => FullVector (TraverseVector f v) where
  identityBuilder (TraverseVector v) = TraverseVector (identityBuilder <$> v)
  negateBuilder (TraverseVector v) = TraverseVector (negateBuilder <$> v)
  scaleBuilder a (TraverseVector v) = TraverseVector (scaleBuilder a <$> v)

mkvar :: VarTracker Int
mkvar = VarTracker $ do
  index <- get
  put (index + 1)
  return index

varList :: Traversable f => f a -> VarTracker (f Int)
varList = traverse (const mkvar)

varList' :: Traversable f => f a -> State Int (f Int)
varList' = unVarTracker . varList

getVar :: forall f r a. BasicVector a => BackGrad r (TraverseVector f a) -> Int -> BackGrad r a
getVar xs i = Linear.lift1_sparse mkbuilder xs
  where
    mkbuilder :: VecBuilder a -> TraverseVector f (VecBuilder a)
    mkbuilder x = TraverseVector (IntMap.singleton i x)

newtype HomoGrad f a = HomoGrad (f a)

instance HasGrad a => HasGrad (HomoGrad f a) where
  type Scalar (HomoGrad f a) = Scalar a
  type Diff (HomoGrad f a) = TraverseVector f (Diff a)
  type Grad (HomoGrad f a) = TraverseVector f (Grad a)

test_r :: MyRecord Float
test_r = MyRecord (10.1, 11.2) [12.3, 13.4, 14.5]

main :: IO ()
main = return ()
