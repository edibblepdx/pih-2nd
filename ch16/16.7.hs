-- Exercise 16.7: Verify the functor laws for the Maybe type. Hint: the proofs
-- proceed by case analysis, and do not require the use of induction.

data Maybe a = Just a | Nothing

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

{-
Functor laws:
  Identity
    fmap id == id
  Composition
    fmap (f . g) == fmap f . fmap g

Case Just a:

  Identity:

    fmap id (Just a)
  =   { applying fmap }
    Just (id a)
  =   { applying id }
    Just a
  =   { unapplying id }
    id (Just a)

  Composition:

    fmap (f . g) (Just a)
  =   { applying fmap }
    Just ((f . g) a)
  =   { applying . }
    Just (f (g a))
  =   { unapplying fmap }
    fmap f (Just (g a))
  =   { unapplying fmap }
    fmap f (fmap g (Just a))
  =   { unapplying . }
    (fmap f . fmap g) (Just a)

Case Nothing:

  Identity:

    fmap id Nothing
  =   { applying fmap }
    Nothing
  =   { unapplying id }
    id Nothing

  Composition:

    fmap (f . g) Nothing
  =   { applying fmap }
    Nothing
  =   { unapplying fmap }
    fmap f Nothing
  =   { unapplying fmap }
    fmap f (fmap g Nothing)
  =   { unapplying . }
    (fmap f . fmap g) Nothing
-}
