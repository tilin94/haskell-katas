module Main where

import Test.Tasty (defaultMain, testGroup)

import Katas11to20 (testKatas11to20)
import Katas1to10 (testKatas1to10)

main :: IO ()
main =
  defaultMain $
  testGroup "Testing HaskellKatas" [testKatas1to10, testKatas11to20]
