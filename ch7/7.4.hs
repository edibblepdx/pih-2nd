-- Exercise 7.4: Using foldl, define a function dec2int :: [Int] -> Int that
-- converts a decimal number into an integer.

-- [2, 3, 4, 5]
-- (2 * 1000) + (3 * 100) + (4 * 10) + (5 * 1)
-- (2 * 1000) + (3 * 100) + (4 * 10) + 5
-- ((2 * 100) + (3 * 10) + 4) * 10 + 5
-- ((2 * 10) + 3) * 10 + 4) * 10 + 5
-- ((0 * 10 + 2) * 10 + 3) * 10 + 4) * 10 + 5

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0
