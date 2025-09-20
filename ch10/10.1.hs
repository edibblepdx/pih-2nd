-- Exercise 10.1: Redefine putStr :: String -> IO () using a list comprehension
-- and the library function sequence_ :: [IO a] -> IO ().

import Prelude hiding (putStr)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x : xs) = do
  putChar x
  putStr xs

putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]
