-- Exercise 7.1: show how the list comprehension [f x | x <- xs, p x] can be
-- re-expressed using the higher-order functions map and filter

func :: (a -> b) -> (a -> Bool) -> [a] -> [b]
func f p xs = [f x | x <- xs, p x]

func' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
func' f p xs = map f (filter p xs)

func'' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
func'' f p = map f . filter p

{-
ghci> func (\x -> 2*x) (\x -> x `mod` 2 == 0) [1,2,3,4]
[4,8]
ghci> func' (\x -> 2*x) (\x -> x `mod` 2 == 0) [1,2,3,4]
[4,8]
ghci> func'' (\x -> 2*x) (\x -> x `mod` 2 == 0) [1,2,3,4]
[4,8]
-}
