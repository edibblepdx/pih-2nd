-- Exercise 4.3: Define a function safetail :: [a] -> [a] that behaves the same
-- as tail except that it maps the empty list to itself rather than producing
-- an error. Using tail and null :: [a] -> bool define safetail using

-- a. a conditional extression
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- b. guarded equations
safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

-- c. pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_ : xs) = xs
