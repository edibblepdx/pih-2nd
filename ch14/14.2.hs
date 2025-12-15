-- Exercise 14.2: In a similar manner, show how a function type a -> b can be
-- made into a monoid provided that the result type b is a monoid.

instance (Monoid b) => Monoid (a -> b) where
  mempty :: a -> b
  mempty = const mempty

  mappend :: (a -> b) -> (a -> b) -> (a -> b)
  mf `mappend` mg = \x -> mf x `mappend` mg x
