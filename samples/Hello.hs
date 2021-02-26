module Main where
import Diff

f :: Floating a => a -> a
f x = x * exp x

x0 :: BVarS Double
x0 = var 1

y0 :: BVarS Double
y0 = f x0

dy :: Double
dy = backpropS y0

main :: IO ()
main = do
    putStrLn ("x0 = " ++ show (bvarValue x0))
    putStrLn ("y0 = " ++ show (bvarValue y0))
    putStrLn ("dy = " ++ show dy)
