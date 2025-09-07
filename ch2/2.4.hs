-- Exercise 2.4: Define a new function `last` in terms of other functions and
-- find another way to do so.

last xs = head (reverse xs)

last' xs = xs !! (length xs - 1)
