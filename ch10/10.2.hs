-- Exercise 10.2: Using recursion, define a version of putBoard :: Board -> IO ()
-- that displays nim boards of any size, rather than being specific to boards
-- with just five rows of stars.

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard = putBoard' 1

putBoard' :: Int -> Board -> IO ()
putBoard' _ [] = return ()
putBoard' r (n : ns) = do
  putRow r n
  putBoard' (r + 1) ns
