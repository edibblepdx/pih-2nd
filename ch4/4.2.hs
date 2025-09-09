-- Exercise 4.2: Define a function third :: [a] -> a that returns the third
-- element in a list that contains at least this many elements using

-- a. head and tail
third :: [a] -> a
third xs = head (tail (tail xs))

-- b. list indexing !!
third' :: [a] -> a
third' xs = xs !! 2

-- c. pattern matching
third'' :: [a] -> a
third'' (_ : _ : c : _) = c
