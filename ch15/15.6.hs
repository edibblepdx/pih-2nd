-- Exercise 15.6: Newton's method for computing the square root of a (non-
-- negative) floating-point number n can be expressed as follows:
--
-- - start with an initial approximation to the result;
-- - given the current approximation a, the next approximation is defined by the
--   function next a = (a + n/a) / 2;
-- - repeat the second step until the two most recent approximations are within
--   some desired distance of one another, at which point the most recent value
--   is returned as the result.
--
-- Define a function sqroot :: Double -> Double that implements this procedure.
-- Hint: first produce an infinite list of approximations using the library
-- function iterate. For simplicity, take the number 1.0 as the initial
-- approximation, and 0.00001 as the distance value.

-- using dropWhile
sqroot :: Double -> Double
sqroot n = snd . head $ dropWhile p $ zip xs (tail xs)
  where
    r = 0.00001
    x0 = 1.0
    xs = iterate next x0
    next a = (a + n / a) / 2.0
    p (x, y) = abs (x - y) > r

-- using list comprehension
sqroot' :: Double -> Double
sqroot' n = head [y | (x, y) <- zip xs (tail xs), p x y]
  where
    r = 0.00001
    x0 = 1.0
    xs = iterate next x0
    next a = (a + n / a) / 2.0
    p x y = abs (x - y) < r
