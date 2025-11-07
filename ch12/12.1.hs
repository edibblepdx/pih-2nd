-- Exercise 12.1: Define an instance of the Functor class for the following type
-- of binary trees that have data in their nodes:

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

{-
Functor laws

1. Preserves identity

Leaf :: Tree a
Node :: Tree a -> a -> Tree a -> Tree a

fmap id Leaf = Leaf
fmap id (Node t1 x t2) = Node (fmap id t1) (id x) (fmap id t2)

2. Preserves function composition

g :: b -> c
h :: a -> b

fmap (g . h) Leaf
  = Leaf
  = fmap h Leaf
  = fmap g (fmap h Leaf)

fmap (g . h) (Node t1 x t2)
  = Node (fmap (g . h) t1) (g (h x)) (fmap (g . h) t1)

(fmap g . fmap h) (Node t1 x t2)
  = fmap g (fmap h (Node t1 x t2))
  = fmap g (Node (fmap h t1) (h x) (fmap h t1))
  = Node (fmap (g . h) t1) (g (h x)) (fmap (g . h) t1)
-}
