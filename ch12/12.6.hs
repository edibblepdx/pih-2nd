-- Exercise 12.6: Define an instance of the Monad class for the type (a ->).

newtype Fun a b = Fun {applyFun :: a -> b}

instance Functor (Fun a) where
  fmap :: (b -> c) -> Fun a b -> Fun a c
  fmap g h = Fun $ g . applyFun h

instance Applicative (Fun a) where
  pure :: b -> Fun a b
  pure b = Fun $ const b

  (<*>) :: Fun a (b -> c) -> Fun a b -> Fun a c
  g <*> h = Fun $ \x -> applyFun g x (applyFun h x)

instance Monad (Fun a) where
  (>>=) :: Fun a b -> (b -> Fun a c) -> Fun a c
  g >>= h = Fun $ \x -> applyFun (h (applyFun g x)) x

{-
instance Monad ((->) a) where
  (>>=) :: (a -> b) -> (b -> a -> c) -> a -> c
  g >>= h = \x -> h (g x) x
-}
