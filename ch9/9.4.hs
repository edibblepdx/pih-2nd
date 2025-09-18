-- Exercise 9.4: Using functions choices, exprs, and eval, verify that there are
-- 33,665,406 possible expressions over numbers 1,3,7,10,25,50 and that only
-- 4,672,540 of these expressions evaluate successfully.

import Countdown

possible :: [Int] -> Int
possible xs = length [e | xs' <- choices xs, e <- exprs xs']

-- ghci> possible [1,3,7,10,25,50]
-- 33665406

successful :: [Int] -> Int
successful xs = length [e | xs' <- choices xs, e <- exprs xs', eval e /= []]

-- ghci > successful [1, 3, 7, 10, 25, 50]
-- 4672540
