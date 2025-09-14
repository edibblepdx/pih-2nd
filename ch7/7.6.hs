-- Exercise 7.6: A higher order function unfold that encapsulates a simple
-- pattern of recursion for producing a list can be defined as follows:
--
-- unfold p h t x | p x       = []
--                | otherwise = h x : unfold p h t (t x)
--
-- int2bin can be rewritten more compactly using unfold as follows:
--
-- int2bin = unfold (==0) (`mod` 2) (`div` 2)
--
-- redefine the functions chop8, map f, and iterate f using unfold

import Prelude hiding (iterate, map)

type Bit = Int

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) (drop 1)

iterate :: (a -> a) -> a -> [a]
iterate = unfold (const False) id

-- same as: iterate f = unfold (const False) id f

{-
ghci> chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
[[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]
ghci> map (\x->x*2) [1,2,3,4,5]
[2,4,6,8,10]
ghci> take 5 (iterate (\x->2*x) 1)
[1,2,4,8,16]
-}
