{- Manually wrapping and unwrapping newtypes inside BVar. -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Main (main, unwrapBVar, wrapBVar) where

import Data.VectorSpace (AdditiveGroup, VectorSpace)
import Downhill.BVar (BVar (BVar))
import Downhill.Grad
  ( Dual (evalGrad),
    HasGrad (Grad, Tang)
  )
import Downhill.Linear.BackGrad (inlineNode)
import Downhill.Linear.Expr (BasicVector (VecBuilder, identityBuilder, sumBuilder))
import Downhill.Metric (MetricTensor (evalMetric, sqrNorm))

newtype MyWrapper a = MyWrapper {unMyWrapper :: a}
  deriving (Semigroup, Monoid, AdditiveGroup, VectorSpace) via a

instance BasicVector a => BasicVector (MyWrapper a) where
  type VecBuilder (MyWrapper a) = MyWrapper (VecBuilder a)
  sumBuilder (MyWrapper x) = MyWrapper (sumBuilder x)
  identityBuilder (MyWrapper a) = MyWrapper (identityBuilder a)

instance Dual da a => Dual (MyWrapper da) (MyWrapper a) where
  evalGrad (MyWrapper da) (MyWrapper a) = evalGrad da a

instance MetricTensor p a => MetricTensor (MyWrapper p) (MyWrapper a) where
  evalMetric (MyWrapper m) (MyWrapper x) = MyWrapper (evalMetric @p m x)
  sqrNorm (MyWrapper m) (MyWrapper x) = sqrNorm @p m x

instance HasGrad a => HasGrad (MyWrapper a) where
  type Tang (MyWrapper a) = MyWrapper (Tang a)
  type Grad (MyWrapper a) = MyWrapper (Grad a)

unwrapBVar :: forall r a. BasicVector (Grad a) => BVar r (MyWrapper a) -> BVar r a
unwrapBVar (BVar x dx) = BVar (unMyWrapper x) (inlineNode MyWrapper dx)

wrapBVar :: forall r a. (BasicVector (Grad a)) => BVar r a -> BVar r (MyWrapper a)
wrapBVar (BVar a da) = BVar (MyWrapper a) (inlineNode unMyWrapper da)

main :: IO ()
main = return ()
