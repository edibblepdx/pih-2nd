-- Exercise 12.2: Complete the following instance declaration to make the
-- partially-applied function type (a ->) into a functor:

instance Functor ((->) a) where
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

{-
when f = (->) a
then f b = (->) a b = a -> b
and  f c = (->) a c = a -> c

Functor laws

1. Preserves identity

fmap id
  = \g -> id . g
  = \g -> (\a -> id (g a))
  = \g -> (\a -> g a)
  = \g -> g
  = id

(g . h) :: b -> c
a :: a -> b

2. Preserves function composition

fmap (g . h)
  = \a -> (g . h) . a
  = \a -> \b -> (g . h) (a b)
  = \a -> \b -> g (h (a b))

g :: b -> c
h :: b -> c
a :: a -> b

fmap g . fmap h
  = (.) g . (.) h
  = (.) g ((.) h)
  = \a -> (.) g (h . a)
  = \a -> g . h . a
  = \a -> \b -> g (h (a b))
-}
