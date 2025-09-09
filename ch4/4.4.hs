-- Show how the || operator can be defined in 4 different ways using pattern
-- matching.

(||) :: Bool -> Bool -> Bool
-- 1.
False || b = b
True || _ = True
-- 2.
True || True = True
True || False = True
False || True = True
False || False = False
-- 3.
False || False = False
_ || _ = True
-- 4.
a || b
  | a == b = b
  | otherwise = True
