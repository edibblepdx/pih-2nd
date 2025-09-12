-- Exercise 6.6: Without looking at the definitions from the standard prelude,
-- define the following library functions on lists using recursion.

import Prelude hiding (and, concat, elem, replicate, (!!))

-- a. Decide if all logical values in a list are True:

and :: [Bool] -> Bool
and [True] = True
and (x : xs)
  | not x = False
  | otherwise = and xs

-- b. Concatenate a list of lists:

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

-- c. Produce a list with n identical elements:

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a | n > 0 = a : replicate (n - 1) a

-- d. Select the nth element of a list:

(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(x : xs) !! n = xs !! (n - 1)

-- e. Decide if a value is an element of a list:

elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem v (x : xs)
  | v == x = True
  | otherwise = elem v xs
