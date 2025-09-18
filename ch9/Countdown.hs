-- Countdown problem

module Countdown where

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- return true if an operation is valid
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- exploiting associativity and identities
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

-- apply an operation
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

-- collect all Val n
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- The singleton pattern is useful to avoid handling of Maybe cases
-- return the result of evaluation an expression
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- subsequences
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

-- interleave
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

-- permutations
perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

-- generate all possible choices of inputs
-- as all permutations of each subsequence
choices :: [a] -> [[a]]
choices = concatMap perms . subs

-- return true if this expression is a solution
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- Brute force solution

-- generate all possible ways to split a list that can be concatenated into the
-- original list
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- generate all expressions whose list of values is the given list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- return all possible expressions that solve the countdown problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- Reject expressions that fail to evaluate

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
    | (ls, rs) <- split ns,
      lx <- results ls,
      ry <- results rs,
      res <- combine' lx ry
  ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]
