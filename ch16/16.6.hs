-- Exercise 16.6: Given the type declaration
--
-- data Tree = Leaf Int | Node Tree Tree
--
-- show that the number of leaves in such a tree is always one greater than the
-- number of nodes by induction on trees. Hint: start by defining functions that
-- count the number of leaves and nodes in a tree.

data Tree = Leaf Int | Node Tree Tree

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

{-
show that leaves = 1 + nodes

Base case:

  leaves (Leaf n) = 1 + nodes (Leaf n)
=   { applying leaves, nodes }
  1 = 1 + 0
=   { applying + }
  1 = 1

Inductive hypothesis:

  leaves l = 1 + nodes l
  leaves r = 1 + nodes r

Inductive case:

  leaves (Node l r) = 1 + nodes (Node l r)
=   { applying leaves, nodes }
  leaves l + leaves r = 1 + 1 + nodes l + nodes r
=   { inductive hypothesis }
  1 + 1 + nodes l + nodes r = 1 + 1 + nodes l + nodes r
-}
