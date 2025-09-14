-- Exercise 7.3: Redefine functions map f and filter p using foldr.

import Prelude hiding (filter, map)

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f []
  where
    f x y
      | p x = x : y
      | otherwise = y

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []
