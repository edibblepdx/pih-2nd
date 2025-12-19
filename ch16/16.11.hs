-- Exercise 16.11: Given the equation comp' e c = comp e ++ c, show how to
-- construct the recursive definition for comp' by induction on e.

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD
  deriving (Show)

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

{-
comp' e c = comp e ++ c

Using the method of making append vanish.

Base case:

  comp' (Val n) c
=   { specification of comp' }
  comp (Val n) ++ c
=   { applying comp }
  [PUSH n] ++ c
=   { applying ++ }
  PUSH n : c

Inductive case:

  comp' (Add x y) c
=   { specification of comp' }
  comp (Add x y) ++ c
=   { applying comp }
  comp x ++ comp y ++ [ADD] ++ c
=   { associativity of ++ }
  comp x ++ (comp y ++ ([ADD] ++ c))
=   { applying ++ }
  comp x ++ (comp y ++ (ADD : c))
=   { induction hypothesis }
  comp x ++ (comp' y (ADD : c))
=   { induction hypothesis }
  comp' x (comp' y (ADD : c))
-}
