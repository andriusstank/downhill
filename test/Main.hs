module Main (main) where
import Test.Hspec (hspec)
import DownhillTest.AD (adTests)

main :: IO ()
main = hspec adTests
