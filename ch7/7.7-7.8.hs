-- Exercise 7.7: Modify the binary string transmitter example to detect simple
-- transmission errors using the concept of parity bits.

import BinaryStringTransmitter
import Data.Char

popcount :: [Bit] -> Int
popcount = sum

-- make9 is similar to make8 but it prepends a parity bit
make9 :: [Bit] -> [Bit]
make9 bits = popcount byte `mod` 2 : byte
  where
    byte = make8 bits

encode' :: String -> [Bit]
encode' = concatMap (make9 . int2bin . ord)

-- chop9 is similar to chop9 but it included the parity bit
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode' :: [Bit] -> String
decode' = map (chr . bin2int . validate) . chop9

-- validate checks the parity bit and panics if it doesn't match otherwise it
-- drops the parity bit
validate :: [Bit] -> [Bit]
validate bits
  | head bits /= popcount byte `mod` 2 = error "parity mismatch"
  | otherwise = byte
  where
    byte = drop 1 bits

transmit' :: String -> String
transmit' = decode' . channel . encode'

-- Exercise 7.8: Test with a faulty communication channel

badchannel :: [Bit] -> [Bit]
badchannel = tail

transmit'' :: String -> String
transmit'' = decode' . badchannel . encode'

{-
ghci> transmit' "higher-order functions are easy"
"higher-order functions are easy"
ghci> transmit'' "higher-order functions are easy"
"*** Exception: parity mismatch
CallStack (from HasCallStack):
  error, called at 7.7-7.8.hs:31:42 in main:Main
-}
