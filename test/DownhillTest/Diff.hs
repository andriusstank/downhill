{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Diff (
    diffTests,
)
where

import Control.Monad (unless)
import Downhill.BVar qualified as BVar
import Downhill.BVar (BVar, DFun(..))
import Downhill.Linear.Fun (LinearMap (..))
import Downhill.Manifold (HasGrad)
import NumHask.Prelude
import Test.HUnit (Assertion, assertString)
import Test.Hspec (SpecWith, describe, it)
import Prelude qualified

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

class (c1 a, c2 a) => C2 c1 c2 a
instance (c1 a, c2 a) => C2 c1 c2 a

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
positiveOptions = defaultOptions{args = [1.5, 0.5, 0.75]}

class Unconstrained a
instance Unconstrained a

{-# ANN diffTests "HLint: ignore Redundant fromInteger" #-}
diffTests :: SpecWith ()
diffTests = describe "Diff" $ do
    it "id" $ testFun @Unconstrained defaultOptions id
    it "const" $ testFun @FromInteger defaultOptions (const (fromInteger 1))
    it "negate" $ testFun @Subtractive defaultOptions negate
    it "+" $ testFun2 @Additive defaultOptions (+)
    it "-" $ testFun2 @Subtractive defaultOptions (-)
    it "*" $ testFun2 @Multiplicative defaultOptions (*)
    it "recip" $ testFun @Divisive defaultOptions recip
    it "/" $ testFun2 @Divisive defaultOptions (/)
    it "exp" $ testFun @ExpField defaultOptions exp
    it "sqrt" $ testFun @ExpField positiveOptions sqrt

    it "sin" $ testFun @TrigField defaultOptions sin
    it "cos" $ testFun @TrigField defaultOptions cos
    it "asin" $ testFun @TrigField defaultOptions{args = [-0.25, 0.0, 0.5]} asin
    it "acos" $ testFun @TrigField defaultOptions{args = [-0.25, 0.0, 0.5]} acos
    it "atan" $ testFun @TrigField defaultOptions atan
    it "sinh" $ testFun @TrigField defaultOptions sinh
    it "cosh" $ testFun @TrigField defaultOptions cosh
    it "asinh" $ testFun @TrigField defaultOptions asinh
    it "acosh" $ testFun @TrigField defaultOptions{args = [1.25, 1.5, 2.0]} acosh
    it "atanh" $ testFun @TrigField defaultOptions{args = [-0.25, 0.0, 0.5]} atanh
    it "atan2" $ testFun2 @TrigField defaultOptions atan2

    it "P.+" $ testFun2 @Prelude.Num defaultOptions (Prelude.+)
    it "P.-" $ testFun2 @Prelude.Num defaultOptions (Prelude.-)
    it "P.negate" $ testFun @Prelude.Num defaultOptions Prelude.negate
    it "P.*" $ testFun2 @Prelude.Num defaultOptions (Prelude.*)
    it "P.abs" $ testFun @Prelude.Num defaultOptions Prelude.abs
    it "P.recip" $ testFun @Prelude.Fractional defaultOptions Prelude.recip
    it "P./" $ testFun2 @Prelude.Fractional defaultOptions (Prelude./)

    it "P.exp" $ testFun @Prelude.Floating defaultOptions Prelude.exp
    it "P.log" $ testFun @Prelude.Floating positiveOptions Prelude.log
    it "P.sin" $ testFun @Prelude.Floating defaultOptions Prelude.sin
    it "P.cos" $ testFun @Prelude.Floating defaultOptions Prelude.cos
    it "P.asin" $ testFun @Prelude.Floating defaultOptions{args = [-0.25, 0.0, 0.5]} Prelude.asin
    it "P.acos" $ testFun @Prelude.Floating defaultOptions{args = [-0.25, 0.0, 0.5]} Prelude.acos
    it "P.atan" $ testFun @Prelude.Floating defaultOptions Prelude.atan
    it "P.sinh" $ testFun @Prelude.Floating defaultOptions Prelude.sinh
    it "P.cosh" $ testFun @Prelude.Floating defaultOptions Prelude.cosh
    it "P.asinh" $ testFun @Prelude.Floating defaultOptions Prelude.asinh
    it "P.acosh" $ testFun @Prelude.Floating defaultOptions{args = [1.25, 1.5, 2.0]} Prelude.acosh
    it "P.atanh" $ testFun @Prelude.Floating defaultOptions{args = [-0.25, 0.0, 0.5]} Prelude.atanh
