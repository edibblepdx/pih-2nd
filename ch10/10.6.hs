-- Exercise 10.6: Using getCh, define an action readLine :: IO String that
-- behaves in the same way as getLine, except that it also permits the delete
-- key to be used to remove characters.

import Control.Monad
import System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

readLine :: IO String
readLine = readLine' []

readLine' :: String -> IO String
readLine' xs = do
  x <- getCh
  case x of
    '\n' -> do
      return xs
    '\DEL' -> do
      if null xs
        then do
          readLine' []
        else do
          putStr "\b \b"
          readLine' (init xs)
    _ -> do
      putChar x
      readLine' (xs ++ [x])
