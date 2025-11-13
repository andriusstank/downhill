module Main (main) where
import Test.Hspec (hspec)
import DownhillTest.AD (adTests)
import DownhillTest.Diff (diffTests)

main :: IO ()
main = hspec $ do
    adTests
    diffTests
