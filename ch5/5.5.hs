-- Exercise 5.5: A triple (x,y,z) of positive integers is a Pythagorean if it
-- satisfies the equation x^2 + y^2 = z^2. Using a list comprehension with three
-- generators, define a function pyths :: Int -> [(Int,Int,Int)] that returns the
-- list of all such triples whose components are at most a given limit.

pyths :: Int -> [(Int, Int, Int)]
pyths n =
  [ (x, y, z)
    | x <- [1 .. n],
      y <- [1 .. n],
      z <- [1 .. n],
      x ^ 2 + y ^ 2 == z ^ 2
  ]
