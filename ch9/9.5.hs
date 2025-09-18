-- Exercise 9.5: Similarly, verify that the number of expressions that evaluate
-- successfully increases to 10,839,369 if the numeric domain is generalized to
-- arbitrary integers.

import Countdown hiding (eval, valid)

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

successful :: [Int] -> Int
successful xs = length [e | xs' <- choices xs, e <- exprs xs', eval e /= []]

-- ghci> successful [1,3,7,10,25,50]
-- 10839369
