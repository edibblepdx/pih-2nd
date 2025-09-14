-- Exercise 7.2: Without looking at the definitions from the standard prelude,
-- define the following higher-order library functions on lists.

import Prelude hiding (all, any, dropWhile, takeWhile)

-- a. Decide if all elements of a list satisfy a predicate:

-- Type in the book is (a -> Bool) -> [Bool] -> Bool
-- Type in the prelude (a -> Bool) -> [a] -> Bool
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- b. Decide if any element of a list satisfies a predicate:

-- Type in the book is (a -> Bool) -> [Bool] -> Bool
-- Type in the prelude (a -> Bool) -> [a] -> Bool

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- c. Select elements from a list while they satisfy a predicate:

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) = if p x then x : takeWhile p xs else []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- d. Remove elements from a list while they satisfy a predicate:

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs) = if p x then dropWhile p xs else x : xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs
