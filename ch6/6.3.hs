-- Exercise 6.3: Define the exponentiation operator ^ for non-negative numbers
-- integers using the same pattern of recursion as the multiplication operator *
-- ans show how the expression 2 ^ 3 is evaluated using your definition.

import Prelude hiding ((^))

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m ^ (n - 1))

{-
2 ^ 3
=   { applying ^ }
2 * (2 ^ 2)
=   { applying ^ }
2 * (2 * (2 ^ 1))
=   { applying ^ }
2 * (2 * (2 * (2 ^ 0)))
=   { applying ^ }
2 * (2 * (2 * 1))
=   { applying * }
8
-}
