module Main where

import Test.Tasty
import Test.Tasty.HUnit

import HaskellKatas
  ( NestedList(Elem, List)
  , elemAt
  , isPalindrome
  , myButLast
  , myFlatten
  , myLast
  , myLength
  , myReverse
  )

main :: IO ()
main =
  defaultMain $
  testGroup
    "Testing HaskellKatas"
    [ testMyLast
    , testMyButLast
    , testElemAt
    , testMyLenght
    , testMyReverse
    , testIsPalindrome
    , testMyFlatten
    ]

testMyLast :: TestTree
testMyLast =
  testGroup
    "Testing myLast"
    [ testCase "Returning the last of a three elements list" $
      myLast [1, 2, 3] @?= Just 3
    , testCase "Returning the last of a single elements list" $
      myLast ["single"] @?= Just "single"
    , testCase "Returning the last of an empty list" $
      myLast ([] :: [Int]) @?= Nothing
    ]

testMyButLast :: TestTree
testMyButLast =
  testGroup
    "Testing myButLast"
    [ testCase "Returning the but last of a four elements list" $
      myButLast [1, 2, 3, 4] @?= Just 3
    , testCase "Returning the but last of a three elements list" $
      myButLast [1, 2, 3] @?= Just 2
    , testCase "Returning the but last of a two elements list" $
      myButLast [1, 2] @?= Just 1
    , testCase "Returning the but last of a single elements list" $
      myButLast ["single"] @?= Nothing
    , testCase "Returning the but last of an empty list" $
      myButLast ([] :: [Int]) @?= Nothing
    ]

testElemAt :: TestTree
testElemAt =
  testGroup
    "Testing elemAt"
    [ testCase "Returning the element at with a four elements list" $
      elemAt ["wrong", "wrong", "wrong", "right"] 3 @?= Just "right"
    , testCase "Returning the element at with a singleton list" $
      elemAt ["single"] 0 @?= Just "single"
    , testCase "Returning the element at with an empty list" $
      elemAt ([] :: [Int]) 0 @?= Nothing
    , testCase "Returning Nothing when index is out of bounds" $
      elemAt ["singleton"] 1 @?= Nothing
    ]

testMyLenght :: TestTree
testMyLenght =
  testGroup
    "Testing myLength"
    [ testCase "Returning the length of a four elements list" $
      myLength [1, 2, 3, 4] @?= 4
    , testCase "Returning the length of a singleton list" $
      myLength ["single"] @?= 1
    , testCase "Returning the length of an empty list" $
      myLength ([] :: [Int]) @?= 0
    ]

testMyReverse :: TestTree
testMyReverse =
  testGroup
    "Testing myReverse"
    [ testCase "Returning the reverse of a four elements list" $
      myReverse [1, 2, 3, 4] @?= [4, 3, 2, 1]
    , testCase "Returning the reverse of a two elements list" $
      myReverse [1, 2] @?= [2, 1]
    , testCase "Returning the reverse of an string" $
      myReverse "Menem" @?= "meneM"
    , testCase "Returning the reverse of a singleton list" $
      myReverse ["single"] @?= ["single"]
    , testCase "Returning the reverse of an empty list" $
      myReverse ([] :: [Int]) @?= []
    ]

testIsPalindrome :: TestTree
testIsPalindrome =
  testGroup
    "Testing isPalindrome"
    [ testCase "Returning True when the list is a palindrome" $
      isPalindrome "menem" @?= True
    , testCase "Returning False when the list is not a palindrome" $
      isPalindrome [1, 2, 3, 4] @?= False
    ]

testMyFlatten :: TestTree
testMyFlatten =
  testGroup
    "Testing myFlatten"
    [ testCase "Returning the flatten of a nested list" $
      myFlatten
        (List
           [List [Elem 1, Elem 2], List [Elem 3, Elem 4], List [Elem 5, Elem 6]]) @?=
      [1, 2, 3, 4, 5, 6]
    , testCase "Returning the flatten of a 2 levels nested list" $
      myFlatten
        (List
           [ List [List [Elem 1], Elem 1, Elem 2]
           , List [Elem 3, List [Elem 4], Elem 4]
           , List [List [Elem 1], Elem 5, Elem 6]
           ]) @?=
      [1, 1, 2, 3, 4, 4, 1, 5, 6]
    ]
