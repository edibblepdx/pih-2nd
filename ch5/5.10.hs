-- Exercise 5.10: Modify the Caeser cipher program to also handle uppercase
-- letters.

import CaeserCipher
import Data.Char

alphas :: String -> Int
alphas xs = length [x | x <- xs, isAlpha x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == toLower x']

freqs :: String -> [Float]
freqs xs = [percent (Main.count x xs) n | x <- ['a' .. 'z']]
  where
    n = alphas xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = Main.freqs xs

-- ghci> Main.crack (encode 3 "HASKELL IS FUN")
-- "HASKELL IS FUN"
