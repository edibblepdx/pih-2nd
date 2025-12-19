-- Exercise 16.8: Given the type and instance declarations below, verify the
-- functor laws for the Tree type, by induction on trees.

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

{-
Functor laws:
  Identity
    fmap id == id
  Composition
    fmap (f . g) == fmap f . fmap g

Identity:

  Base case:

    fmap id (Leaf x)
  =   { applying fmap }
    Leaf (id x)
  =   { applying id }
    Leaf x
  =   { unapplying id }
    id (Leaf x)

  Inductive case:

    fmap id (Node l r)
  =   { applying fmap }
    Node (fmap id l) (fmap id r)
  =   { inductive hypothesis }
    Node l r
  =   { unapplying id }
    id (Node l r)

Composition:

  Base case:

    fmap (f . g) (Leaf x)
  =   { applying fmap }
    Leaf ((f . g) x)
  =   { applying . }
    Leaf (f (g x))
  =   { unapplying fmap }
    fmap f (Leaf (g x))
  =   { unapplying fmap }
    fmap f (fmap g (Leaf x))
  =   { unapplying . }
    (fmap f . fmap g) (Leaf x)

  Inductive case:

    fmap (f . g) (Node l r)
  =   { applying fmap }
    Node (fmap (f . g) l) (fmap (f . g) r)
  =   { inductive hypothesis }
    Node ((fmap f . fmap g) l) ((fmap f . fmap g) r)
  =   { applying . }
    Node (fmap f (fmap g l)) (fmap f (fmap g r))
  =   { unapplying fmap }
    fmap f (Node (fmap g l) (fmap g r))
  =   { unapplying fmap }
    fmap f (fmap g (Node l r))
  =   { unapplying . }
    (fmap f . fmap g) (Node l r)
-}
