-- Exercise 4.7: Luhn algorithm.

luhnDouble :: Int -> Int
luhnDouble x = if y > 9 then y - 9 else y
  where
    y = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w =
  (sum (map luhnDouble [x, z]) + y + w) `mod` 10 == 0

luhn' :: Int -> Int -> Int -> Int -> Bool
luhn' x y z w =
  (luhnDouble x + y + luhnDouble z + w) `mod` 10 == 0
