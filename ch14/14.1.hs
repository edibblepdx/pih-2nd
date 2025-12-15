-- Exercise 14.1: Complete the following declaration from Data.Monoid to make a
-- pair type into a monoid provided the two component types are monoids:

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty :: (a, b)
  mempty = (mempty, mempty)

  mappend :: (a, b) -> (a, b) -> (a, b)
  (mx1, my1) `mappend` (mx2, my2) = (mx1 `mappend` mx2, my1 `mappend` my2)
