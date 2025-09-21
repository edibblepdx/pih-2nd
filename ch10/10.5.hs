-- Exercise 10.5: Redefine adder using sequence :: [IO a] -> IO [a] that performs
-- a list of actions and returns a list of the resulting values.

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- readLn
  t <- adder' n
  putStrLn ("The total is " ++ show (sum t))

adder' :: Int -> IO [Int]
adder' n = sequence [readLn | _ <- [1 .. n]]
