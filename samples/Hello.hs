{- Very simple example that computes value and
   derivative of function f(x) = x*exp(x) at point x=1.5
-}
module Main where
import Downhill.BVar (BVar(BVar, bvarValue), var, backprop)

f :: Floating a => a -> a
f x = x * exp x

x0 :: BVar Double Double
x0 = var 1.5

y0 :: BVar Double Double
y0 = f x0

dy :: Double
dy = backprop y0 1

main :: IO ()
main = do
  putStrLn ("x0 = " ++ show (bvarValue x0))
  putStrLn ("y0 = " ++ show (bvarValue y0))
  putStrLn ("dy = " ++ show dy)
