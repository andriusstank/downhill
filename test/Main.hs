module Main (main) where
import Test.Hspec (hspec)
import DownhillTest.AD (adTests)
import DownhillTest.Diff (diffTests)
import DownhillTest.Record (recordTests)

main :: IO ()
main = hspec $ do
    adTests
    diffTests
    recordTests
