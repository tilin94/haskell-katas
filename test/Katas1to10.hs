module Katas1to10
  ( testKatas1to10
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import HaskellKatas1to10
  ( NestedList(Elem, List)
  , compress
  , elemAt
  , encode
  , isPalindrome
  , myButLast
  , myFlatten
  , myLast
  , myLength
  , myReverse
  , pack
  )

testKatas1to10 :: TestTree
testKatas1to10 =
  testGroup
    "Testing Katas from 1 to 10"
    [ testMyLast -- 			1
    , testMyButLast -- 		2
    , testElemAt -- 		  3
    , testMyLenght -- 		4
    , testMyReverse -- 		5
    , testIsPalindrome -- 6
    , testMyFlatten -- 		7
    , testCompress -- 		8
    , testPack -- 				9
    , testEncode -- 			10
    ]

-- 1
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

-- 2
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

-- 3
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

-- 4
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

-- 5
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

-- 6
testIsPalindrome :: TestTree
testIsPalindrome =
  testGroup
    "Testing isPalindrome"
    [ testCase "Returning True when the list is a palindrome" $
      isPalindrome "menem" @?= True
    , testCase "Returning False when the list is not a palindrome" $
      isPalindrome [1, 2, 3, 4] @?= False
    ]

-- 7
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
    , testCase "Returning the flatten of a singleton list" $
      myFlatten (List [Elem 1]) @?= [1]
    , testCase "Returning the flatten of an empty list" $
      myFlatten (List []) @?= ([] :: [Int])
    ]

-- 8
testCompress :: TestTree
testCompress =
  testGroup
    "Testing compress"
    [ testCase "Returning 'ABC' when 'AABBBCC' is provided" $
      compress "AABBBCC" @?= "ABC"
    , testCase "Returning 'A' when 'AAA' is provided" $ compress "AAA" @?= "A"
    , testCase "Returning '' when '' is provided" $ compress "" @?= ""
    ]

-- 9
testPack :: TestTree
testPack =
  testGroup
    "Testing pack"
    [ testCase "Returning 'ABC' when 'AABBBCC' is provided" $
      pack "AABBBCC" @?= ["AA", "BBB", "CC"]
    , testCase "Returning 'A' when 'AAA' is provided" $ pack "AAA" @?= ["AAA"]
    , testCase "Returning '' when '' is provided" $ pack "" @?= []
    ]

-- 10
testEncode :: TestTree
testEncode =
  testGroup
    "Testing encode"
    [ testCase "Returning 'ABC' when 'AABBBCC' is provided" $
      encode "AABBBCC" @?= [(2, 'A'), (3, 'B'), (2, 'C')]
    , testCase "Returning 'A' when 'AAA' is provided" $
      encode "AAA" @?= [(3, 'A')]
    , testCase "Returning '' when '' is provided" $ encode "" @?= []
    ]
