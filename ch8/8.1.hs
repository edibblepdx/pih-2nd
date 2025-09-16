-- Exercise 8.1: In a similar manner to the function add, define a recursive
-- function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers.

data Nat = Zero | Succ Nat
  deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

{-
mult 3 2
=   { applying mult }
add 2 (mult 2 2)
=   { applying mult }
add 2 (add 2 (mult 1 2))
=   { applying mult }
add 2 (add 2 (add 2 (mult 0 2)))
=   { applying mult }
add 2 (add 2 (add 2 0))
=   { applying add }
6
-}

{-
mult 0 2
=   { applying mult }
0
-}

{-
mult 2 0
=   { applying mult }
add 0 (mult 1 0)
=   { applying mult }
add 0 (add 0 (mult 0 0))
=   { applying mult }
add 0 (add 0 0)
=   { applying add }
0
-}
