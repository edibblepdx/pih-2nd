-- Exercise 5.4: Show how the library function replicate :: Int -> a -> [a] that
-- produces a list of identical elements can be defined using a list
-- comprehension.

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1 .. n]]
