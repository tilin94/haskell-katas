module Katas11to20
  ( testKatas11to20
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import HaskellKatas11to20 (encodeToString)

testKatas11to20 :: TestTree
testKatas11to20 = testGroup "Testing Katas from 1 to 10" [testEncodeToString]

testEncodeToString :: TestTree
testEncodeToString =
  testGroup
    "Testing encodeToString"
    [ testCase "Returning 'ABC' when 'AABBBCC' is provided" $
      encodeToString [(2, 'A'), (3, 'B'), (2, 'C')] @?= "ABC"
    , testCase "Returning 'A' when 'AAA' is provided" $
      encodeToString [(3, 'A')] @?= "A"
    , testCase "Returning '' when '' is provided"
      $ encodeToString ([] :: [(Int, Char)]) @?= ""
    ]
