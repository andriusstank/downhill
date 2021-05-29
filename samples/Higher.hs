{-# LANGUAGE TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}

module Main where
import Diff
import Data.VectorSpace (VectorSpace(..))
import BVar.Num (AsNum(AsNum, unAsNum))
import Downhill.Linear.Expr (FullVector)

f :: Floating a => a -> a
f x = sin (2*x)

x0 :: BVarS (BVarS Double)
x0 = var 0

f' :: forall a. (Floating a) => BVarS (AsNum a) -> BVarS (AsNum a)
f' = f @(BVarS (AsNum a))

checkFloating :: Floating (BVarS Double) => ()
checkFloating = ()

checkFullVector :: FullVector (BVarS Double) => ()
checkFullVector = ()

y0 :: BVarS (AsNum (BVarS Double))
y0 = undefined

dy :: BVarS Double
dy = unAsNum (backpropS y0)

dyy :: Double
dyy = backpropS dy

main :: IO ()
main = do
    putStrLn ("x0 = " ++ show (bvarValue . bvarValue $ x0))
    putStrLn ("y0 = " ++ show (bvarValue . unAsNum . bvarValue $ y0))
    putStrLn ("dy = " ++ show (bvarValue dy))
    putStrLn ("dyy = " ++ show dyy)
