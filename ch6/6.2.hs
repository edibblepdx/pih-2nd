-- Exercise 6.2: Define a recursive function sumdown :: Int -> Int that returns
-- the sum of the non-negative integers from a given value down to zero.

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n - 1)
