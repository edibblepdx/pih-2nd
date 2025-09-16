-- Exercise 8.3: Consider the following type of binary trees:
--
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--
-- Let us say that a tree is balanced if the number of leaves in the left and
-- right subtree of every node differs by at most one, with the leaves themselves
-- being trivially balanced. Define a function balanced :: Tree a -> Bool that
-- decides if a binary tree is balanced or not.

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- balanced
t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))

-- balanced
t2 :: Tree Int
t2 = Node (Leaf 1) (Node (Leaf 6) (Leaf 9))

-- unbalanced
t3 :: Tree Int
t3 = Node (Leaf 1) (Node (Node (Leaf 7) (Leaf 8)) (Leaf 9))

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) =
  abs (numLeaves l - numLeaves r) <= 1
    && balanced l
    && balanced r

numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node l r) = numLeaves l + numLeaves r
