-- Exercise 10.4: Define an action adder :: IO () that reads a given number of
-- integers from the keyboard, one per line, and displays their sum.

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- readLn
  sum <- adder' 0 n
  putStrLn ("The total is " ++ show sum)

adder' :: Int -> Int -> IO Int
adder' sum 0 = return sum
adder' sum left = do
  n <- readLn
  adder' (sum + n) (left - 1)
