-- Exercise 6.9: Using the five-step process, construct the library functions
-- that:

import Prelude hiding (last, sum, take)

-- a. Calculate the sum of a list of numbers

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

sum' :: [Int] -> Int
sum' xs = foldr (+) 0 xs

-- b. Take a given number of elements from the start of a list

-- I think n > length of the list should be a precondition
take :: Int -> [a] -> [a]
take 0 _ = []
take n (x : xs) = x : take (n - 1) xs

-- c. Select the last element of a non-empty list

last :: [a] -> a
last [x] = x
last (x : xs) = last xs
