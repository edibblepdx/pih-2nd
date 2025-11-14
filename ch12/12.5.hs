-- Exercise 12.5: Work out the types for the variables in the four applicative
-- laws.

{-
1. pure id <*> x   = x

  pure id :: f (a -> a)
  pure id <*> x :: f a

  x :: f a

2. pure (g x)      = pure g <*> pure x

  pure (g x) :: f (g x)

  pure g :: f g
  pure x :: f x
  pure g <*> pure x :: f (g x)

3. x <*> pure y    = pure (\g -> g y) <*> x

  x :: f (a -> b)
  y :: a

  pure y :: f a
  x <*> pure y :: f b

  pure (\g -> g y) :: f (\g -> g y)
  pure (\g -> g y) <*> x = f (x y) :: f b

4. x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

  x :: b -> c
  y :: a -> b
  z :: a

  y <*> z :: f b
  x <*> (y <*> z) :: f c

  (.) :: (b -> c) -> (a -> b) -> a -> c

  pure (.) <*> x <*> y :: f (a -> c)
  (pure (.) <*> x <*> y) <*> z :: f c
-}
