-- Exercise 8.2: The standard prelude defines
--
-- data Ordering = LT | EQ | GT
--
-- compare :: Ord a => a -> a -> Ordering
--
-- that decides if one value of an ordered type is less than (LT), equal to (EQ),
-- or greater than (GT) another value. Using this function, redefine the function
-- occurs :: Ord a => a -> Tree a -> Bool for search trees. Why is this new
-- definition more efficient than the original version.

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: (Ord a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) =
  case compare x y of
    EQ -> True
    LT -> occurs x l
    GT -> occurs x r

-- This version only requires one comparison of x and y whereas the previous
-- version may require two comparisons.
