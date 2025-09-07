-- Exercise 2.1: book examples and using ghci

-- Double of an number:
double :: (Num x) => x -> x
double x = x + x

-- Quadruple of an number:
quadruple :: (Num x) => x -> x
quadruple x = double (double x)

-- Factorial of a positive integer:
factorial n = product [1 .. n]

-- Average of a list of integers:
average ns = sum ns `div` length ns
