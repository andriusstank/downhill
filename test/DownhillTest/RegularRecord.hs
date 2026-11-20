module DownhillTest.RegularRecord (regularRecordTests) where

import DownhillTest.RegularRecord.BVar (bvarTests)
import DownhillTest.RegularRecord.Fields (fieldsTests)
import DownhillTest.RegularRecord.Structure (structureTests)
import DownhillTest.RegularRecord.Workflow1 (workflow1Tests)
import DownhillTest.RegularRecord.Workflow2 (workflow2Tests)
import Test.Hspec (SpecWith, describe)

regularRecordTests :: SpecWith ()
regularRecordTests = describe "RegularRecord" $ do
    fieldsTests
    structureTests
    bvarTests
    workflow1Tests
    workflow2Tests

