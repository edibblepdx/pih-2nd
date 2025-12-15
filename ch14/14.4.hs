-- Exercise 14.4: In a similar manner, show how the following type of binary
-- trees can be made into a foldable and traversable type.

import Data.Foldable

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Functor)

instance Foldable Tree where
  fold :: (Monoid a) => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l <> x <> fold r

  foldMap :: (Monoid b) => (a -> b) -> Tree a -> b
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z Leaf = z
  foldr f z (Node l x r) = foldr f (foldr f (f x z) r) l

  foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f z Leaf = z
  foldl f z (Node l x r) = foldl f (foldl f (f z x) l) r

instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f Leaf = pure Leaf
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
