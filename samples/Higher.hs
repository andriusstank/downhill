{-# LANGUAGE TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}

module Main where
import Data.VectorSpace (VectorSpace(..))
import Downhill.BVar.Num (AsNum(AsNum, unAsNum), NumBVar, backpropNum, numbvarValue)
import Downhill.Linear.Expr (FullVector)
import Downhill.DVar (var)

f :: Floating a => a -> a
f x = sin (2*x)

x0 :: NumBVar (NumBVar Double)
x0 = var 0

f' :: forall a. (Floating a) => NumBVar a -> NumBVar a
f' = f @(NumBVar a)

checkFloating :: Floating (NumBVar Double) => ()
checkFloating = ()

checkFullVector :: FullVector (NumBVar Double) => ()
checkFullVector = ()

y0 :: NumBVar (AsNum (NumBVar Double))
y0 = undefined

dy :: NumBVar Double
dy = unAsNum (backpropNum y0)

dyy :: Double
dyy = backpropNum dy

main :: IO ()
main = do
    putStrLn ("x0 = " ++ show (numbvarValue . numbvarValue $ x0))
    putStrLn ("y0 = " ++ show (numbvarValue . unAsNum . numbvarValue $ y0))
    putStrLn ("dy = " ++ show (numbvarValue dy))
    putStrLn ("dyy = " ++ show dyy)
