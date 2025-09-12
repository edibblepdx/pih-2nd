-- Exercise 6.4: Define a recursive function euclid :: Int -> Int -> Int that
-- implements Euclid's algorithm for calculating the greatest common divisor
-- of two non-negative integers.

euclid :: Int -> Int -> Int
euclid n m | n == m = n
           | n > m = euclid m (n-m)
           | n < m = euclid n (m-n)
