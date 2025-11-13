{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.Diff (
    diffTests,
)
where

import Control.Monad (unless)
import Downhill.BVar (BVar, DFun (DFun))
import Downhill.BVar qualified as BVar
import Downhill.Linear.Fun (LinearFun (..))
import NumHask.Prelude
import Test.HUnit (Assertion, assert, assertString, (@=?))
import Test.Hspec (SpecWith, describe, it)
import Downhill.Manifold (HasGrad)

data NumDiffOpts = NumDiffOpts
    { h :: Double
    , tol :: Double
    , args :: [Double]
    }

testFun :: forall c. (c Double, forall r. c (BVar r Double)) => NumDiffOpts -> (forall x. (c x) => x -> x) -> Assertion
testFun opts f = traverse_ testAt opts.args
  where
    DFun f' = BVar.ad bvarF
        where
            bvarF :: forall r. BVar r Double -> BVar r Double
            bvarF = f
    testAt :: Double -> Assertion
    testAt x0 = do
         checkDy (df <*| 1) 
         checkDy (1 |*> df) 
      where
        (_, df) = f' x0
        checkDy :: Double -> Assertion
        checkDy =
            let
                x1 = x0 - opts.h
                x2 = x0 + opts.h
                numDy = (f x2 - f x1) / (x2 - x1)
             in
                \dy -> unless (abs (dy - numDy) < opts.tol) (assertString (show dy ++ " /= " ++ show numDy))

class IsDouble a where
    fromDouble :: Double -> a
instance IsDouble Double where
    fromDouble = id
instance (IsDouble a, HasGrad a) => IsDouble (BVar r a) where
    fromDouble = BVar.constant . fromDouble

class (c1 a, c2 a) =>  C2 c1 c2 a
instance (c1 a, c2 a) =>  C2 c1 c2 a

testFun2 :: forall c. (c Double, forall r. c (BVar r Double)) => NumDiffOpts -> (forall x. (c x) => x -> x -> x) -> Assertion
testFun2 opts f = do
    traverse_ testFirst opts.args
    traverse_ testSecond opts.args
 where
    testFirst, testSecond :: Double -> Assertion
    testFirst x = testFun @(C2 c IsDouble) opts (fromDouble x `f`)
    testSecond y = testFun @(C2 c IsDouble) opts (`f` fromDouble y)

defaultOptions :: NumDiffOpts
defaultOptions = NumDiffOpts{h = 1.0e-9, tol = 1.0e-6, args = [-1.5, -0.5, 0.75]}

positiveOptions :: NumDiffOpts
positiveOptions = NumDiffOpts{h = 1.0e-9, tol = 1.0e-6, args = [1.5, 0.5, 0.75]}

class Unconstrained a
instance Unconstrained a


{-# ANN diffTests "HLint: ignore Redundant fromInteger" #-}

diffTests :: SpecWith ()
diffTests = describe "Diff" $ do
    it "id" $ testFun @Unconstrained defaultOptions id
    it "const" $ testFun @FromInteger defaultOptions (const (fromInteger 1))
    it "negate" $ testFun @Subtractive defaultOptions negate
    it "plus" $ testFun2 @Additive defaultOptions (+)
    it "minus" $ testFun2 @Subtractive defaultOptions (-)
    it "*" $ testFun2 @Multiplicative defaultOptions (*)
    it "recip" $ testFun @Divisive defaultOptions recip
    it "/" $ testFun2 @Divisive defaultOptions (/)
    it "exp" $ testFun @ExpField defaultOptions exp
    it "sqrt" $ testFun @ExpField positiveOptions sqrt
