-- Exercise 5.6: A positive integer is perfect if it equals the sum of all its
-- factors, excluding the number itself. Using a list comprehension and the
-- function factors, define a function perfects :: Int -> [Int] that returns the
-- list of all perfect numbers up to a given limit.

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- My first attempt
perfects :: Int -> [Int]
perfects n = [x | (xs, x) <- zip xss [2 .. n], sum (f xs) == x]
  where
    xss = [factors x | x <- [2 .. n]]
    f xs = take (length xs - 1) xs

-- My second attempt
-- `f` is just `init` from the standard prelude
-- and also not needlessly zipping each number with its factors
perfects' :: Int -> [Int]
perfects' n = [x | x <- [2 .. n], sum (init (factors x)) == x]
