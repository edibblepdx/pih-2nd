-- Exercise 7.5: Without looking at the definitions from the standard prelude,
-- define the higher-order library function curry that converts a function on
-- pairs into a curried function, and conversely, the function uncurry that
-- converts a curried function with two arguments into a function on pairs.

import Prelude hiding (curry, uncurry)

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y
