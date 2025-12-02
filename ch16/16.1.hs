-- Exercise 16.1: Show that add n (Succ m) = Succ (add n m), by induction on n.

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

{-
Base case:

  add Zero (Succ m)
=   { applying add }
  Succ m
=   { unapplying add }
  Succ (add Zero m)

Inductive case:

  add (Succ n) (Succ m)
=   { applying add }
  Succ (add n (Succ m))
=   { induction hypothesis }
  Succ (Succ (add n m))
=   { unapplying add }
  Succ (add (Succ n) m)
-}
