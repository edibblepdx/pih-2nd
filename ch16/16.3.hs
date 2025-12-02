-- Exercise 16.3: Using the following definition for the library function that
-- decides if all elements of a list satisfy a predicate

all p [] = True
all p (x : xs) = p x && all p xs

-- Complete the proof of the correctness of replicate by showing that it produces
-- a list with identical elements, all (== x) (replicate n x), by induction on
-- n >= 0. Hint: show that the property is always True.

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

{-
Properties:

  1. length (replicate n x) = n

Base case:

  all (== x) (replicate 0 x)
=   { applying replicate }
  all (== x) []
=   { applying all }
  True

Inductive case:

  all (== x) (replicate (n+1) x)
=   { applying replicate }
  all (== x) (x : replicate n x)
=   { applying all }
  x == x && all (== x) (replicate n x)
=   { applying == }
  True && all (== x) (replicate n x)
=   { induction hypothesis }
  True && True
=   { applying && }
  True
-}
