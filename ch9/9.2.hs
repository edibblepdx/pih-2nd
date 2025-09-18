-- Exercise 9.2: Define a recursive function
-- isChoice :: Eq a => [a] -> [a] -> Bool that decides if one list is chosen from
-- another, without using the combinatorial functions perms and subs.

isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice [x] [y] = x == y
isChoice (x : xs) ys = isChoice xs (removeFirst x ys)

removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y : ys)
  | x == y = ys
  | otherwise = y : removeFirst x ys
