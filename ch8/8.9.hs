-- Exercise 8.9: Extend the abstract machine to support the use of multiplication.

-- Expression
data Expr = Val Int | Add Expr Expr | Mul Expr Expr | Div Expr Expr

-- Control Stack
type Cont = [Op]

-- Operation
data Op = EVAL Expr (Int -> Op) | ADD Int | MUL Int | DIV Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y ADD : c)
eval (Mul x y) c = eval x (EVAL y MUL : c)
eval (Div x y) c = eval x (EVAL y DIV : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y op : c) n = eval y (op n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MUL n : c) m = exec c (n * m)
exec (DIV n : c) m = exec c (n `div` m)

-- Evaluate in control stack
value :: Expr -> Int
value e = eval e []

e1 :: Expr -- 20
e1 = Mul (Val 5) (Val 4)

e2 :: Expr -- 9
e2 = Add (Mul (Val 2) (Val 2)) (Val 5)

e3 :: Expr -- 4
e3 = Div (Val 8) (Val 2)
