-- Exercise 12.8: Rather than making a parameterised type into an instance of
-- the Functor, Applicative, and Monad classes in this order, in practice it is
-- sometimes simple to define the functor and applicative instances in terms of
-- the monad instance, relying on the fact that the order in which declarations
-- are mad is not important in Haskell.

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
    x <- st
    return (g x)

instance Applicative ST where
  pure :: a -> ST a
  pure x = S (x,)

  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    x <- stx
    return (f x)

{-
stf <*> stx =
  stf >>= \f ->
  stx >>= \x ->
  return (f x)
-}

instance Monad ST where
  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S $ \s ->
    let (x, s') = app st s in app (f x) s'
