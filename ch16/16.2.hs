-- Exercise 16.2: Using this property, together with add n Zero = n, show that
-- addition is commutative, add n m = add m n, by induction on n.

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

{-
Properties:

  1. add n (Succ m) = Succ (add n m)
  2. add n Zero = n

Base case:

  add Zero m
=   { applying add }
  m
=   { unapplying property 2 }
  add m Zero

Inductive case:

  add (Succ n) m
=   { applying add }
  Succ (add n m)
=   { inductive hypothesis }
  Succ (add m n)
=   { unapplying property 1 }
  add m (Succ n)
-}
