-- Exercise 6.7: Define a recursive function merge :: Ord a => [a] -> [a] -> [a]
-- that merges two sorted lists to give a single sorted list.

-- xs and ys being sorted is a precondition of merge
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys

-- Exercise 6.8: Using merge, define a function msort :: Ord a => [a] -> [a] that
-- implements merge sort. First define a function halve :: [a] -> ([a], [a]) that
-- splits a list into two halves whose lengths differ by at most one.

halve :: [a] -> ([a], [a])
halve xs = splitAt len xs
  where
    len = length xs `div` 2

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where
    (ys, zs) = halve xs

{-
msort [1,3,2,7,9,5,4]
=   { applying msort }
merge (msort [1,3,2]) (msort [7,9,5,4])
=   { applying msort }
merge (merge (msort [1]) (msort [3,2])) (msort [7,9,5,4])
=   { applying msort }
merge (merge [1] (msort [3,2])) (msort [7,9,5,4])
=   { applying msort }
merge (merge [1] (merge (msort [3]) (msort [2]))) (msort [7,9,5,4])
=   { applying msort }
merge (merge [1] (merge [3] [2]) (msort [7,9,5,4])
=   { applying merge }
merge (merge [1] [2,3]) (msort [7,9,5,4])
=   { applying merge }
merge [1,2,3] (msort [7,9,5,4])
=   { applying msort }
merge [1,2,3] (merge (msort [7,9]) (msort [5,4]))
=   { applying msort }
merge [1,2,3] (merge (merge (msort [7]) (msort [9])) (msort [5,4]))
=   { applying msort }
merge [1,2,3] (merge (merge [7] [9])) (msort [5,4]))
=   { applying merge }
merge [1,2,3] (merge [7,9] (msort [5,4]))
=   { applying msort }
merge [1,2,3] (merge [7,9] (merge (msort [5]) (msort [4])))
=   { applying msort }
merge [1,2,3] (merge [7,9] (merge [5] [4]))
=   { applying merge }
merge [1,2,3] (merge [7,9] [4,5])
=   { applying merge }
merge [1,2,3] [4,5,7,9]
=   { applying merge }
[1,2,3,4,5,7,9]
-}
