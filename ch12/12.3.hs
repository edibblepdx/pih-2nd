-- Exercise 12.3: Define an instance of the Applicative class for the type
-- (a ->). If you are familiar with combinatory logic, you might recognize pure
-- and <*> for this type as being the well-known K and S combinators.

instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure = const

  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x)

{-
Applicative laws

1. Preserves identity

pure id <*> x
  = (\_ -> id) <*> x
  = \y -> (\_ -> id) y (x y)
  = \y -> id (x y)
  = \y -> id (x y)
  = x

2. Preserves function application

pure (g x) = \y -> g x

pure g <*> pure x
  = (\_ -> g) <*> (\_ -> x)
  = \y -> (\_ -> g) y ((\_ -> x) y)
  = \y -> (\_ -> g) y x
  = \y -> g x

3. When an effectful function is applied to a pure argument, the order in which
   we evaluate the two components doesn't matter

x <*> pure y
  = x <*> (\_ -> y)
  = \z -> x z ((\_ -> y) z)
  = \z -> x z y

pure (\g -> g y) <*> x
  = (\_ -> \g -> g y) <*> x
  = \z -> (\_ -> \g -> g y) z (x z)
  = \z -> (\g -> g y) (x z)
  = \z -> (x z) y
  = \z -> x z y

4. Modulo the types that are involved, the operator <*> is associative

x <*> (y <*> z)
  = \g -> x g ((y <*> z) g)
  = \g -> x g ((\h -> y h (z h)) g)
  = \g -> x g (y g (z g))

(pure (.) <*> x <*> y) <*> z
  = ((pure (.) <*> x) <*> y) <*> z
  = (((\_ -> (.)) <*> x) <*> y) <*> z
  = (\g -> (\_ -> (.)) g (x g)) <*> y <*> z
  = (\h -> (\g -> (\_ -> (.)) g (x g)) h (y h)) <*> z
  = (\h -> (\g -> (.) (x g)) h (y h)) <*> z
  = (\h -> (.) (x h) (y h)) <*> z
  = (\h -> \u -> x h (y h u)) <*> z
  = \v -> (\h -> \u -> x h (y h u)) v (z v)
  = \v -> (\u -> x v (y v u)) (z v)
  = \v -> x v (y v (z v))
-}
