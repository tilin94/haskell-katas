module HaskellKatas
  ( myLast
  , myButLast
  , elemAt
  , myLength
  , myReverse
  , isPalindrome
  , myFlatten
  , NestedList(Elem, List)
  ) where

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast xs = pure . head . reverse $ xs

-- Beautiful example from solutions
-- myButLast :: [a] -> a
-- myButLast = last . init
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [_] = Nothing
myButLast xs = pure . head . tail . reverse $ xs

elemAt :: [a] -> Int -> Maybe a
elemAt [] _ = Nothing
elemAt (x:_) 0 = Just x
elemAt (_:xs) n = elemAt xs (n - 1)

-- Beautiful example from solutions
-- myLength :: [a] -> Int
-- myLength = sum . map (\_->1)
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a
  = Elem a
  | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List xs) = concatMap myFlatten xs
