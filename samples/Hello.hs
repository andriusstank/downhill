module Main where
import Downhill.BVar.Num

f :: Floating a => a -> a
f x = x * exp x

x0 :: NumBVar Double
x0 = var 1

y0 :: NumBVar Double
y0 = f x0

dy :: Double
dy = backpropNum y0

main :: IO ()
main = do
    putStrLn ("x0 = " ++ show (numbvarValue x0))
    putStrLn ("y0 = " ++ show (numbvarValue y0))
    putStrLn ("dy = " ++ show dy)
