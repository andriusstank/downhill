{-# LANGUAGE TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}

module Main where
import Diff
import Notensor (FullVector)
import Data.VectorSpace (VectorSpace(..))
import Affine (AsNum(AsNum))

f :: Floating a => a -> a
f x = sin (2*x)

x0 :: BVarS (BVarS Double)
x0 = var 0

f' :: forall a. (Floating a) => BVarS (AsNum a) -> BVarS (AsNum a)
f' x = f @(BVarS (AsNum a)) x

checkFloating :: Floating (BVarS Double) => ()
checkFloating = ()

checkFullVector :: FullVector (BVarS Double) => ()
checkFullVector = ()

y0 :: BVarS (BVarS Double)
y0 = undefined

dy :: BVarS Double
dy = backpropS y0

dyy :: Double
dyy = backpropS dy

main :: IO ()
main = do
    putStrLn ("x0 = " ++ show (bvarValue . bvarValue $ x0))
    putStrLn ("y0 = " ++ show (bvarValue . bvarValue $ y0))
    putStrLn ("dy = " ++ show (bvarValue dy))
    putStrLn ("dyy = " ++ show dyy)
