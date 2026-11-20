{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import DownhillTest.AD (adTests)
import DownhillTest.Diff (diffTests)
import DownhillTest.Rec (recTests)
import DownhillTest.Record (recordTests)
import DownhillTest.RegularRecord (regularRecordTests)
import DownhillTest.Tree (treeTests)
import DownhillTest.Tree.Workflow2 (treeWorkflow2Tests)
import Test.Hspec (hspec, Spec)

import NumHask.Prelude
import DownhillTest.VSpace.Scalar (testScalar4)
import DownhillTest.VSpace.V2 (v2Tests)
import DownhillTest.VSpace.M2 (m2Tests)
import Data.Constraint (Dict(Dict))

vspaceTests :: Spec
vspaceTests = do
    Dict <- testScalar4
    v2Tests
    m2Tests

otherTests :: Spec
otherTests = do
    adTests
    diffTests
    recTests
    recordTests
    regularRecordTests
    treeTests
    treeWorkflow2Tests

main :: IO ()
main = hspec $ do
    vspaceTests
    otherTests
