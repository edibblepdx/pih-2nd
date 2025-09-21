-- Exercise 10.3: In a similar manner to the first exercise, redefine the
-- generalized version of putBoard using a list comprehension and sequence_.

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard b = sequence_ [putRow r n | (r, n) <- zip [1 ..] b]
