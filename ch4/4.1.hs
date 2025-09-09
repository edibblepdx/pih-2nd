-- Exercise 4.1: Using library functions, define a function called
-- halve :: [a] -> ([a],[a]) that splits an even length list into two halves.

-- If you want to ignore odd-length lists
halve :: [a] -> ([a], [a])
halve xs = if even n then splitAt (n `div` 2) xs else ([], [])
  where
    n = length xs

-- If you don't want to ignore odd-length lists
halve' :: [a] -> ([a], [a])
halve' xs = splitAt n xs
  where
    n = length xs `div` 2
