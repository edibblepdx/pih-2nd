-- Exercise 7.9: Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- that alternately applies its two argument functions to successive elements in
-- a list, in turn about order. For example:
--
-- altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- Exercise 7.10: using altMap, defien a function luhn :: [Int] -> Bool that
-- implements the Luhn algorithm from the exercises in chapter 4 for bank card
-- numbers of any length.

luhnDouble :: Int -> Int
luhnDouble x = if y > 9 then y - 9 else y
  where
    y = x * 2

-- These are the same but the second uses composition.

luhn :: [Int] -> Bool
luhn ns = sum (altMap id luhnDouble (reverse ns)) `mod` 10 == 0

luhn' :: [Int] -> Bool
luhn' = (== 0) . (`mod` 10) . sum . altMap id luhnDouble . reverse
