{- Higher order derivatives -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main(main) where

import Downhill.BVar (var)
import Downhill.BVar.Num (backpropNum, AsNum (AsNum))

-- Alternatively, the type can be rearrange this way:
-- diff ::
--   (forall b. Floating b => b -> b) ->
--   (forall a. Floating a => a -> a)
diff :: forall a. Floating a => (forall b. Floating b => b -> b) -> a -> a
diff fun x0 = backpropNum (fun (var (AsNum x0)))

f :: Floating a => a -> a
f x = exp (2 * x)

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
