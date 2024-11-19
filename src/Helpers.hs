module Helpers
  ( printKata
  , printKataWith2Args
  ) where

printKata ::
     (Show arg, Show result) => String -> (arg -> result) -> arg -> IO ()
printKata fnName fn arg = do
  let result = fn arg
  putStrLn (fnName ++ " of " ++ show arg ++ " is " ++ show result)

printKataWith2Args ::
     (Show result, Show arg1, Show arg2)
  => String
  -> (arg1 -> arg2 -> result)
  -> arg1
  -> arg2
  -> IO ()
printKataWith2Args fnName fn arg1 arg2 = do
  let result = fn arg1 arg2
  putStrLn
    (fnName ++
     " of " ++ show arg1 ++ " and " ++ show arg2 ++ " is " ++ show result)
