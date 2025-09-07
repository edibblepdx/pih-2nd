-- Exercise 2.5: Define a new function `init` in terms of other functions
-- that removes the last element of a non-empty list and find another way to
-- do so.

init xs = take (length xs - 1) xs

init' xs = reverse (tail (reverse xs))
