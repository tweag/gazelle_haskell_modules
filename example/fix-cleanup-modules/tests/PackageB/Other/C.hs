module PackageB.Other.C where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import PackageB.Exposed.A ()
import PackageB.Other.D ()

test_dummy :: TestTree
test_dummy = testCase "dummy test" $
    return ()
