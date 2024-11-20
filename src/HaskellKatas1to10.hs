module HaskellKatas1to10
  ( myLast
  , myButLast
  , elemAt
  , myLength
  , myReverse
  , isPalindrome
  , myFlatten
  , compress
  , pack
  , encode
  , NestedList(Elem, List)
  ) where

import Data.List (group)

-- 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast xs = pure . head . reverse $ xs

-- 2
-- Beautiful example from solutions
-- myButLast :: [a] -> a
-- myButLast = last . init
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [_] = Nothing
myButLast xs = pure . head . tail . reverse $ xs

-- 3
elemAt :: [a] -> Int -> Maybe a
elemAt [] _ = Nothing
elemAt (x:_) 0 = Just x
elemAt (_:xs) n = elemAt xs (n - 1)

-- 4
-- Beautiful example from solutions
-- myLength :: [a] -> Int
-- myLength = sum . map (\_->1)
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a
  = Elem a
  | List [NestedList a]

-- 7
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List xs) = concatMap myFlatten xs

-- 8
-- eliminate consecutive duplicates
compress :: Eq a => [a] -> [a]
compress = map head . group

-- 9
pack :: Eq a => [a] -> [[a]]
pack = group

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . group
