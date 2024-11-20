module Main where

import HaskellKatas
  ( compress
  , elemAt
  , isPalindrome
  , myButLast
  , myLast
  , myLength
  , myReverse
  )

import Helpers (printKata, printKataWith2Args)

main :: IO ()
main = do
  let sampleList = [1, 2, 3] :: [Int]
  printKata "myReverse" myReverse sampleList
  printKata "myLength" myLength sampleList
  printKata "myLast" myLast sampleList
  printKata "myButLast" myButLast sampleList
  printKata "isPalindrome" isPalindrome sampleList
  printKataWith2Args "elemAt" elemAt sampleList 1
