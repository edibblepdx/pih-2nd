-- Exercise 16.4: Using the definition
--
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)
--
-- verify the following two properties, by induction on xs:
--
-- xs ++ [] = xs
-- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
--
-- Hint: the proofs are similar to those for the add function.

{-
xs ++ [] = xs

Base case:

  [] ++ []
=   { applying ++ }
  []

Inductive case:

  (x:xs) ++ []
=   { applying ++ }
  x : (xs ++ [])
=   { inductive hypothesis }
  x : xs

===================================

xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

Base case:

  [] ++ (ys ++ zs)
=   { applying ++ }
  ys ++ zs
=   { unapplying ++ }
  ([] ++ ys) ++ zs

Inductive case:

  (x:xs) ++ (ys ++ zs)
=   { applying ++ }
  x : (xs ++ (ys ++ zs))
=   { inductive hypothesis }
  x : ((xs ++ ys) ++ zs)
=   { unapplying ++ }
  (x : (xs ++ ys)) ++ zs
=   { unapplying ++ }
  ((x:xs) ++ ys) ++ zs
-}
