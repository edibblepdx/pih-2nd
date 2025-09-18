-- Exercise 9.1: Redefine the combinatorial function choices using a list
-- comprehension rather than using composition, concat and map

import Countdown hiding (choices)

choices :: [a] -> [[a]]
choices xs = [ps | ss <- subs xs, ps <- perms ss]
