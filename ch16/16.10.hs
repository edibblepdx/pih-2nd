-- Exercise 16.10: Verify the monad laws for the list type. Hint: the proofs can
-- be completed using simple properties of list comprehensions.

instance Monad [] where
  return x = [x]
  xs >>= f = [y | x <- xs, y <- f x]

{-
Monad Laws:
  Left identity
    return a >>= k = k a
  Right identity
    m >>= return = m
  Associativity
    m >>= (\x -> k x >>= h) = (m >>= k) >>= h

List comprehension properties:
\*These are names that I came up with
\*Induction is unnecessary

  Empty
    [y | x <- [], y <- f x] = []
  Singleton
    [y | x <- [a], y <- f x] = f a
  Identity
    [y | y <- xs] = xs
  Recursive Identity
    [y | x <- xs, y <- [x]] = xs
  Flatten
    [z | y <- [y | x <- xs, y <- f x], z <- g y]
      = [z | x <- xs, y <- f x, z <- g y]

Left identity:

  return a >>= k
=   { applying return }
  [a] >>= k
=   { applying >>= }
  [y | x <- [a], y <- k x]
=   { singleton property }
  k a

Right identity

    xs >>= return
  =   { applying >>= }
    [y | x <- xs, y <- return x]
  =   { applying return }
    [y | x <- xs, y <- [x]]
  =   { recursive identity property }
    xs

Associativity

    xs >>= (\x -> k x >>= h)
  =   { applying >>= }
    [y | x <- xs, y <- (\x -> k x >>= h) x]
  =   { applying lambda }
    [y | x <- xs, y <- k x >>= h]
  =   { applying >>= }
    [y | x <- xs, y <- [y | z <- k x, y <- h z]]
  =   { flatten property }
    [y | x <- xs, z <- k x, y <- h z]
  =   { unflatten property }
    [y | z <- [z | x <- xs, z <- k x], y <- h z]
  =   { unapplying >>= }
    [y | z <- (xs >>= k), y <- h z]
  =   { unapplying >>= }
    (xs >>= k) >>= h
-}
