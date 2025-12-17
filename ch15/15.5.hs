-- Exercise 15.5: Define appropriate versions of the library functions
--
-- repeat :: a -> [a]
-- repeat x = xs where xs = x : xs
--
-- take :: Int -> [a] -> [a]
-- take 0 _ = []
-- take _ [] = []
-- take n (x : xs) = x : take (n - 1) xs
--
-- replicate :: Int -> a -> [a]
-- replicate n = take n . repeat
--
-- for the following type of binary trees:

import Prelude hiding (repeat, replicate, take)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

repeat :: a -> Tree a
repeat x = ts where ts = Node ts x ts

take :: Int -> Tree a -> Tree a
take 0 _ = Leaf
take _ Leaf = Leaf
take n (Node l x r) = Node (take (n - 1) l) x (take (n - 1) r)

replicate :: Int -> a -> Tree a
replicate n = take n . repeat
