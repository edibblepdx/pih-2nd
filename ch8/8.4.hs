-- Exercise 8.4: Define a function balance :: [a] -> Tree a that converts a non-
-- empty list into a balanced tree.

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) =
  abs (numLeaves l - numLeaves r) <= 1
    && balanced l
    && balanced r

numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node l r) = numLeaves l + numLeaves r

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where
    (ys, zs) = split xs

split :: [a] -> ([a], [a])
split xs = splitAt (length xs `div` 2) xs
