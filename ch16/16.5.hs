-- Exercise 16.5: Using the above definition for ++, together with
--
-- take 0 _      = []
-- take _ []     = []
-- take n (x:xs) = x : take (n-1) xs
--
-- drop 0 xs     = xs
-- drop _ []     = []
-- drop n (_:xs) = drop (n-1) xs
--
-- show that take n xs ++ drop n xs = xs, by simultaneous induction on the
-- integer n >= 0 and the list xs. Hint: there are three cases, one for each
-- pattern of arguments in the definition of take and drop.

{-
Base case:

  take 0 xs ++ drop 0 xs
=   { applying take, drop }
  [] ++ xs
=   { applying ++ }
  xs

Base case:

  take n [] ++ drop n []
=   { applying take, drop }
  [] ++ []
=   { applying ++ }
  []

Inductive case

  take (n+1) (x:xs) ++ drop (n+1) (x:xs)
=   { applying take, drop }
  (x : take n xs) ++ drop n xs
=   { applying ++ }
  x : (take n xs ++ drop n xs)
=   { inductive hypothesis }
  x : xs
-}
