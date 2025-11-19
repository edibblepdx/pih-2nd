-- Exercise 12.7: Given the following type of epressions
--
-- data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
--               deriving Show
--
-- that contain the variables of some type a, show how to make this type into
-- instances of the Functor, Applicative, and Monad classes. With the aid of an
-- example, explain what the >>= operator for this type does.

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap _ (Val n) = Val n
  fmap g (Add e1 e2) = Add (g <$> e1) (g <$> e2)

instance Applicative Expr where
  pure :: a -> Expr a
  pure = Var

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Var g <*> expr = g <$> expr
  Val n <*> _ = Val n
  Add g h <*> expr = Add (g <*> expr) (h <*> expr)

instance Monad Expr where
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= g = g x
  Val n >>= _ = Val n
  Add x y >>= g = Add (x >>= g) (y >>= g)

-- A variable can change
-- A value cannot change

-- The bind (>>=) operator in the Monad instance for Expr allows you to replace
-- all Variables, but leave all Values.

a :: Expr Int
a = Add (Var 3) (Val 5)

-- Add one to the variable in a Var Expr
-- ghci> a >>= b
-- Add (Var 4) (Val 5)

b :: Int -> Expr Int
b n = Var (n + 1)

-- Add the variable to itself in an Add Expr
-- ghci> a >>= c
-- Add (Add (Var 3) (Var 3)) (Val 5)

c :: Int -> Expr Int
c n = Add (Var n) (Var n)

-- Add one to the variable in a Var Expr,
-- then add the variable to itself in an Add Expr
-- ghci> a >>= b >>= c
-- Add (Add (Var 4) (Var 4)) (Val 5)

go :: Expr Int
go = Var 5 >>= return

go' :: Expr Int
go' = Val 5 >>= return
