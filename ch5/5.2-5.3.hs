-- Exercise 5.2: Suppose that a coordinate grid of size m x n is given by the
-- list of all pairs (x,y) of integers such that 0 <= x <= m and 0 <= y <= n.
-- Using a list comprehension, define a function
-- grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid of a given
-- size.

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- Exercise 5.3: Using a list comprehension and the function grid above, define a
-- function square :: Int -> [(Int,Int)] that returns a coordinate square of size
-- n, excluding the diagonal from (0,0) to (n,n).

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]
  where
    list = [0 .. n]
