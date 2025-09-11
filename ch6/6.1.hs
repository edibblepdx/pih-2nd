-- Exercise 6.1: How does the recursive factorial function behave if applied to
-- a negative argument? Modify the definition to prohibit negative arguments by
-- add a guard to the recursive case.

-- The function does not terminate.

factorial :: Int -> Int
factorial 0 = 1
factorial n
  | n < 0 = 1
  | otherwise = n * factorial (n - 1)

-- The book solution was this:

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * factorial (n - 1)
