-- Exercise 14.5: Using foldMap, define a generic version of the higher order
-- function filter on lists that can be used with any foldable type:

filterF :: (Foldable t) => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> [x | p x])
