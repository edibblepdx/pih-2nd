-- Exercise 9.3: What effect would generalizing the function split to also return
-- pairs containing the empty list have on the behavior of solutions.

import Countdown hiding (exprs, solutions, split)

-- generalization of split
split :: [a] -> [([a], [a])]
split [] = []
split xs = split' 0 xs
  where
    split' n xs
      | n <= length xs = splitAt n xs : split' (n + 1) xs
      | otherwise = []

-- solutions will not terminate now because recursive calls to exprs because the
-- length of the list is not reduced.
--
-- you call expr [original] which calls split and the first pair is
-- ([empty], [original]) then call twice expr [empty], expr [original] which does
-- the same recursive call and goes on forever since the second element of the
-- first tuple returned from split is not reduced.

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
