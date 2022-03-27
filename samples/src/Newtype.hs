{- Manually wrapping and unwrapping newtypes inside BVar. -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Main(main, unwrapBVar, wrapBVar) where

import Data.VectorSpace (AdditiveGroup, VectorSpace)
import Downhill.BVar (BVar (BVar))
import Downhill.Grad (Dual (evalGrad), HasGrad (Grad, MScalar, Metric, Tang), MetricTensor (MtCovector, MtVector, evalMetric, sqrNorm))
import Downhill.Linear.BackGrad ( inlineNode )
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder), FullVector (identityBuilder, negateBuilder, scaleBuilder))

newtype MyWrapper a = MyWrapper { unMyWrapper :: a }
    deriving (Semigroup, Monoid, AdditiveGroup, VectorSpace) via a

instance BasicVector a => BasicVector (MyWrapper a) where
  type VecBuilder (MyWrapper a) = MyWrapper (VecBuilder a)
  sumBuilder (MyWrapper x) = MyWrapper (sumBuilder x)

instance Dual s da a => Dual s (MyWrapper da) (MyWrapper a) where
  evalGrad (MyWrapper da) (MyWrapper a) = evalGrad da a

instance FullVector a => FullVector (MyWrapper a) where
  identityBuilder (MyWrapper a) = MyWrapper (identityBuilder a)
  negateBuilder (MyWrapper a) = MyWrapper (negateBuilder a)
  scaleBuilder x (MyWrapper a) = MyWrapper (scaleBuilder x a)

instance MetricTensor s a => MetricTensor s (MyWrapper a) where
  type MtVector (MyWrapper a) = MyWrapper (MtVector a)
  type MtCovector (MyWrapper a) = MyWrapper (MtCovector a)
  evalMetric (MyWrapper m) (MyWrapper x) = MyWrapper (evalMetric m x)
  sqrNorm (MyWrapper m) (MyWrapper x) = sqrNorm m x

instance HasGrad a => HasGrad (MyWrapper a) where
  type MScalar (MyWrapper a) = MScalar a
  type Tang (MyWrapper a) = MyWrapper (Tang a)
  type Grad (MyWrapper a) = MyWrapper (Grad a)
  type Metric (MyWrapper a) = MyWrapper (Metric a)

unwrapBVar :: forall r a. BasicVector (Grad a) => BVar r (MyWrapper a) -> BVar r a
unwrapBVar (BVar x dx) = BVar (unMyWrapper x) (inlineNode MyWrapper dx)

wrapBVar :: forall r a. (BasicVector (Grad a)) => BVar r a -> BVar r (MyWrapper a)
wrapBVar (BVar a da) = BVar (MyWrapper a) (inlineNode unMyWrapper da)

main :: IO ()
main = return ()
