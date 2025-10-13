-- TODO:
-- Exercise 11.1: Using the function gametree, verify that there are 549,946
-- nodes in the complete game tree for a 3 x 3 tic-tac-toe game starting from
-- the empty grid, and that the maximum depth of this tree is 9.

-- TODO:
-- Exercise 11.2: Our tic-tac-toe program always chooses the first move from the
-- list of best moves. Modify the final program to choose a random move from the
-- list of best moves, using the function randomRIO :: (Int,Int) -> IO Int from
-- System.Random to generate a random integer in the given range.

import Prelude
import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)

-- Basic declarations

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid utilities

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (notElem B)

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- Displaying a grid

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

{--
showRow [O, B, X]
=   { applying showRow }
beside (interleave bar (map showPlayer [O, B, X]))
=   { applying map }
beside (interleave bar
  [["   ", " O ", "   "],
   ["   ", "   ", "   "],
   ["   ", " X ", "   "]]
=   { applying interleave }
beside
  [["   ", " O ", "   "], ["|", "|", "|"],
   ["   ", "   ", "   "], ["|", "|", "|"],
   ["   ", " X ", "   "]]
]
=   { applying beside }
foldr1 (zipWith (++))
  [["   ", " O ", "   "], ["|", "|", "|"],
   ["   ", "   ", "   "], ["|", "|", "|"],
   ["   ", " X ", "   "]]
=   { applying foldr1 }
  zipWith (++) ["   ", " O ", "   "]
  (zipWith (++) ["|", "|", "|"]
  (zipWith (++) ["   ", "   ", "   "]
  (zipWith (++) ["|", "|", "|"] ["   ", " X ", "   "])))
=   { applying zipWith}
  zipWith (++) ["   ", " O ", "   "]
  (zipWith (++) ["|", "|", "|"]
  (zipWith (++) ["   ", "   ", "   "] ["|   ", "| X ", "|   "]))
=   { applying zipWith}
  zipWith (++) ["   ", " O ", "   "]
  (zipWith (++) ["|", "|", "|"] ["   |   ", "   | X ", "   |   "])
=   { applying zipWith}
  zipWith (++) ["   ", " O ", "   "] ["|   |   ", "|   | X ", "|   |   "]
=   { applying zipWith}
  ["   |   |   ",
   " O |   | X ",
   "   |   |   "]
--}

-- Making a move

valid :: Grid -> Int -> Bool
valid g i = 0 <= 1 && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = [chop size (xs ++ [p] ++ ys) | valid g i]
  where
    (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a number

getNat :: String -> IO Int
getNat prompt = do
  putStrLn prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then
      return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

-- Human vs human

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStrLn ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "ERROR: Invalid move"
          run' g p
        [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

-- Game trees

data Tree a = Node a [Tree a]
  deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

-- Pruning the tree

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

-- Minmax algorithm

minmax :: Tree Grid -> Tree (Grid, Player)
minmax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minmax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minmax ts
    ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minmax tree

-- Human vs computer

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty O

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "ERROR: Invalid move"
          play' g p
        [g'] -> play g' (next p)
  | p == X = do
      putStr "Player X is thinking... "
      (play $! bestmove g p) (next p)
