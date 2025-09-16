-- Exercise 8.5: Given the type declaration
--
-- data Expr = Val Int | Add Expr Expr
--
-- define a higher-order function
--
-- folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
--
-- such that folde f g replaces each Val constructor in an expression by the
-- function f, and each Add constructor by the function g.

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Exercise 8.6: Using folde, define a function eval :: Expr -> Int that evaluates
-- an expression to an integer value, and a function size :: Expr -> Int that
-- calculates the number of values in an expression.

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

e :: Expr
e = Add (Add (Val 2) (Val 3)) (Val 4) -- 9
