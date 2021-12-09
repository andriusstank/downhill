{- Higher order derivatives -}
{-# LANGUAGE TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}

module Main where
import Data.VectorSpace (VectorSpace(..))
import Downhill.BVar.Num (AsNum(AsNum, unAsNum), NumBVar, backpropNum, numbvarValue)
import Downhill.Linear.Expr (FullVector)
import Downhill.BVar (var, BVar (BVar), backprop)
import Downhill.Grad (HasGrad(MScalar, Grad))

-- Alternatively, diff :: (forall b. Floating b => (b -> b)) -> (forall a. Floating a => a -> a)
diff :: forall a. Floating a => (forall b. Floating b => (b -> b)) -> a -> a
diff f x0 = backpropNum (f (var (AsNum x0)))

f :: Floating a => a -> a
f x = exp (2*x)

f' :: Floating a => a -> a
f' = diff f

f'' :: Floating a => a -> a
f'' = diff f'

f''' :: Floating a => a -> a
f''' = diff f''

main :: IO ()
main = do
    let x0 = 0 :: Double
    putStrLn ("x0 = " ++ show x0)
    putStrLn ("y0 = " ++ show (f x0))
    putStrLn ("dy = " ++ show (f' x0))
    putStrLn ("dyy = " ++ show (f'' x0))
    putStrLn ("dyyy = " ++ show (f''' x0))
