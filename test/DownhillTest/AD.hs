{-# LANGUAGE  NoImplicitPrelude  #-}
module DownhillTest.AD(adTests)
where
import NumHask.Prelude
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (describe, it, SpecWith)
import Downhill.Linear.AD (AD, adDense)
import Downhill.Linear.Builder (Dense)
import Downhill.Linear.Fun (LinearFun(..))
import Downhill.Internal.Graph.Bigraph (DenseBigraph)

zeroTest :: Assertion
zeroTest = do
    let
        z :: AD (Dense Int) (Dense Int)
        z = zero
        dz :: DenseBigraph Int Int
        dz = adDense z
    dz <*| 0 @=? 0
    0 |*> dz @=? 0

adTests :: SpecWith ()
adTests = describe "zero" (it "?" zeroTest)
