-- Exercise 9.6: Modify the final program to:
--
-- a. Allow the use of exponentiation in expressions;
-- b. Produce the nearest solutions if no exact solution is possible;
-- c. Order the solutions using a suitable measure of simplicity.

main :: IO ()
main = print (solutions' [1, 2, 3, 4] 16)

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

-- return true if an operation is valid
valid :: Op -> Integer -> Integer -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y /= 1

-- apply an operation
apply :: Op -> Integer -> Integer -> Integer
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Integer | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

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

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

type Result = (Expr, Integer)

combine :: Result -> Result -> [Result]
combine (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Integer] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
    | (ls, rs) <- split ns,
      lx <- results ls,
      ry <- results rs,
      res <- combine lx ry
  ]

-- return all possible expressions that solve the countdown problem
solutions :: [Integer] -> Integer -> [Expr]
solutions ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

solutions' :: [Integer] -> Integer -> [Expr]
solutions' ns n = case [r | ns' <- choices ns, r <- results ns'] of
  [] -> []
  ((e, m) : rs) -> nearest rs (abs (m - n)) n [e]

nearest :: [Result] -> Integer -> Integer -> [Expr] -> [Expr]
nearest [] _ _ es = qsort es
nearest ((e, m) : rs) d n es
  | d' == d = nearest rs d n (e : es)
  | d' < d = nearest rs d' n [e]
  | otherwise = nearest rs d n es
  where
    d' = abs (m - n)

-- sorting by number of operators

size :: Expr -> Int
size (Val _) = 1
size (App _ l r) = size l + size r

qsort :: [Expr] -> [Expr]
qsort [] = []
qsort (x : xs) = smaller ++ [x] ++ larger
  where
    smaller = qsort [a | a <- xs, size a <= size x]
    larger = qsort [b | b <- xs, size b > size x]
