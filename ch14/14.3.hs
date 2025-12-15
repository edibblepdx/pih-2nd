-- Exercise 14.3: Show how the Maybe type can be made foldable and traversable,
-- by giving explicit definitions for fold, foldMap, foldr, foldl, and traverse.

import Data.Foldable

instance Foldable Maybe where
  fold :: (Monoid a) => Maybe a -> a
  fold (Just x) = x
  fold Nothing = mempty

  foldMap :: (Monoid b) => (a -> b) -> Maybe a -> b
  foldMap f (Just x) = f x
  foldMap f Nothing = mempty

  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr f z (Just x) = f x z
  foldr f z Nothing = z

  foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl f z (Just x) = f z x
  foldl f z Nothing = z

instance Traversable Maybe where
  traverse :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse f (Just x) = Just <$> f x
  traverse f Nothing = pure Nothing
