-- Exercise 16.9: Verify the applicative laws for the Maybe type.

data Maybe a = Just a | Nothing

instance Applicative Maybe where
  pure = Just

  Just f <*> m = fmap f m
  Nothing <*> m = Nothing

{-
Applicative laws:
  Identity
    pure id <*> v = v
  Composition
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  Homomorphism
    pure f <*> pure x = pure (f x)
  Interchange
    u <*> pure y = pure ($ y) <*> u

Identity:

  Case Just a:

    pure id <*> Just a
  =   { applying pure }
    Just id <*> Just a
  =   { applying <*> }
    fmap id (Just a)
  =   { applying fmap }
    Just (id a)
  =   { applying id }
    Just a

  Case Nothing:

    pure id <*> Nothing
  =   { applying pure }
    Just id <*> Just a
  =   { applying <*> }
    fmap id Nothing
  =   { applying fmap }
    Nothing

Composition:

  Case Nothing

    pure (.) <*> Nothing <*> v <*> w
  =   { applying pure }
    Just (.) <*> Nothing <*> v <*> w
  =   { applying <*> }
    fmap (.) Nothing <*> v <*> w
  =   { applying fmap }
    Nothing <*> v <*> w
  =   { applying <*> }
    Nothing <*> w
  =   { applying <*> }
    Nothing
  =   { unapplying <*> }
    Nothing <*> (v <*> w)

  Case Just a, Nothing:

    pure (.) <*> Just a <*> Nothing <*> w
  =   { applying pure }
    Just (.) <*> Just a <*> Nothing <*> w
  =   { applying <*> }
    fmap (.) (Just a) <*> Nothing <*> w
  =   { applying fmap }
    Just ((.) a) <*> Nothing <*> w
  =   { applying <*> }
    fmap ((.) a) Nothing <*> w
  =   { applying fmap }
    Nothing <*> w
  =   { applying <*> }
    Nothing
  =   { unapplying fmap }
    fmap a Nothing
  =   { unapplying <*> }
    Just a <*> Nothing
  =   { unapplying <*> }
    Just a <*> (Nothing <*> w)

  case just a, just b:

    pure (.) <*> just a <*> just b <*> w
  =   { applying pure }
    just (.) <*> just a <*> just b <*> w
  =   { applying <*> }
    fmap (.) (just a) <*> just b <*> w
  =   { applying fmap }
    just ((.) a) <*> just b <*> w
  =   { applying <*> }
    fmap ((.) a) (just b) <*> w
  =   { applying fmap }
    just (a . b) <*> w
  =   { applying <*> }
    fmap (a . b) w
  =   { functor composition law for w }
    (fmap a . fmap b) w
  =   { applying . }
    fmap a (fmap b w)
  =   { unapplying <*> }
    fmap a (just b <*> w)
  =   { unapplying <*> }
    just a <*> (just b <*> w)

Homomorphism:

  pure f <*> pure x
=   { applying pure }
  Just f <*> Just x
=   { applying <*> }
  fmap f (Just x)
=   { applying fmap }
  Just (f x)
=   { unapplying pure }
  pure (f x)

Interchange:

  Case Just a:

    Just a <*> pure y
  =   { applying pure }
    Just a <*> Just y
  =   { applying <*> }
    fmap a (Just y)
  =   { applying fmap }
    Just (a y)
  =   { unapplying fmap }
    fmap ($ y) (Just a)
  =   { unapplying <*> }
    Just ($ y) <*> Just a
  =   { unapplying pure }
    pure ($ y) <*> Just a

  Case Nothing:

    Nothing <*> pure y
  =   { applying <*> }
    Nothing
  =   { unapplying fmap }
    fmap ($ y) Nothing
  =   { unapplying <*> }
    Just ($ y) <*> Nothing
  =   { unapplying pure }
    pure ($ y) <*> Nothing
-}
