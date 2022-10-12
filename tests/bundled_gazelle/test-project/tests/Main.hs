module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Fibo
import Power

main = defaultMain dummyTests

dummyTests :: TestTree
dummyTests =
  testGroup "All tests"
    [ testGroup "Fibonacci"
        [ testCase "version 1" $ fibonacci 6 @?= 8,
          testCase "version 2" $ betterFibo 8 @?= 21,
          testCase "version 3" $ fiboRecTerm 15 @?= 610
        ],
      testGroup "Power"
        [ testCase "version 1" $ pow 2 5 @?= 32,
          testCase "version 2" $ karatsuba 2 13 @?= 8192
        ]
    ]
