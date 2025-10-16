import Data.Char
import Data.List
import Data.Ord
import System.IO
-- import System.Random hiding (next)
import Prelude

-- Exercise 11.1: Using the function gametree, verify that there are 549,946
-- nodes in the complete game tree for a 3 x 3 tic-tac-toe game starting from
-- the empty grid, and that the maximum depth of this tree is 9.

-- INFO: exercise 11.1
maxdepth' :: Int
maxdepth' = maxdepth (gametree empty O)

-- INFO: exercise 11.1
maxdepth :: Tree a -> Int
maxdepth (Node _ []) = 0
maxdepth (Node _ ts) = 1 + maximum (map maxdepth ts)

-- Exercise 11.2: Our tic-tac-toe program always chooses the first move from the
-- list of best moves. Modify the final program to choose a random move from the
-- list of best moves, using the function randomRIO :: (Int,Int) -> IO Int from
-- System.Random to generate a random integer in the given range.

-- Exercsie 11.3: Alternatively, modify the final program to choose a move that
-- attempts to take the quickest route to a win, by calculating the depths of
-- resulting game trees and selecting a move that results in a tree with the
-- smallest depth.

-- INFO: exercise 11.3
shortestmove :: Grid -> Player -> Grid
shortestmove g p = mindepth ts best
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minmax tree

-- INFO: exercise 11.3
mindepth :: [Tree (Grid, Player)] -> Player -> Grid
mindepth ts p = fst (minimumBy (comparing snd) tds)
  where
    tds = [(g', maxdepth t) | t@(Node (g', p') _) <- ts, p == p']

-- WARN: Exercise 11.4 is incompatible with exercise 11.2 and 11.3

-- Exercise 11.4: Modify the final program to:
-- a. Let the user decide if they wish to play first or second;
-- b. Allow the length of a winning line to also be changed;
-- c. Generate the game tree once, rather than for each move;
-- d. TODO: Reduce the side of game tree using alpha-beta pruning.

-- Basic declarations

size :: Int
size = 3

-- INFO: Exercise 4.b
line :: Int
line = 2

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

-- INFO: Exercise 4.b
wins :: Player -> Grid -> Bool
wins p g = any (all (== p)) (winLines g)

-- INFO: Exercise 4.b
winLines :: Grid -> [[Player]]
winLines g = rows ++ cols ++ dias
  where
    rows = subs g
    cols = subs (transpose g)
    dias = subs (diags g) ++ subs (diags (map reverse g))

-- INFO: Exercise 4.b
subs :: [[Player]] -> [[Player]]
subs ps =
  [take line (drop n p) | p <- ps, length p >= size, n <- [0 .. size - line]]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. line - 1]]

-- INFO: Exercise 4.b
diags :: Grid -> [[Player]]
diags = init . tail . fn []
  where
    fn xs rows = case rows of
      [] -> remaining xs
      (r : rs) -> map head xs : fn (r : map tail xs) rs
    remaining [] = []
    remaining xs = [y | y : _ <- xs] : remaining [ys | _ : ys <- xs]

{-
-- grid
[[1,2,3],
 [4,5,6],
 [7,8,9]]

-- store            -- return value
[[1,2,3]]           -> []
[[2,3],[4,5,6]]     -> [[1]]
[[3],[5,6],[7,8,9]] -> [[1], [2,4]]
[[6],[8,9]]         -> [[1], [2,4], [3,5,7]]
[[9]]               -> [[1], [2,4], [3,5,7], [6,8]]
[]                  -> [[1], [2,4], [3,5,7], [6,8], [9]]
-}

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
interleave _ [] = []
interleave _ [y] = [y]
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

data Tree a = Node a [Tree a] | Null
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

-- INFO: exercise 11.2
bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minmax tree

-- INFO: Exercise 11.4.a
minmax' :: Player -> Tree Grid -> Tree (Grid, Player)
minmax' p (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minmax' p (Node g ts)
  | turn g == p = Node (g, minimum ps) ts'
  | otherwise = Node (g, maximum ps) ts'
  where
    ts' = map (minmax' p) ts
    ps = [p | Node (_, p) _ <- ts']

-- Human vs computer

-- INFO: Exercise 11.4.a.c
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  p <- player
  let tree = minmax' p (gametree empty p)
  play p tree

-- INFO: Exercise 11.4.a
player :: IO Player
player = do
  putStr "first (1) or second (2): "
  order <- getLine
  case order of
    "1" -> return O
    "2" -> return X
    _ -> do
      putStrLn "ERROR: Invalid player order"
      player

-- INFO: Exercise 11.4.b
lineLength :: IO Int
lineLength = do
  putStr "line length (0 < l <= size): "
  len <- read <$> getLine
  if 0 < len && len <= size
    then
      return len
    else do
      putStrLn "ERROR: Invalid line length"
      lineLength

-- INFO: Exercise 11.4.c
play :: Player -> Tree (Grid, Player) -> IO ()
play player tree@(Node (g, _) _) = do
  cls
  goto (1, 1)
  putGrid g
  play' player tree

-- INFO: Exercise 11.4.c
play' :: Player -> Tree (Grid, Player) -> IO ()
play' player tree@(Node (grid, bestPlayer) ts)
  | wins O grid = putStrLn "Player O wins!\n"
  | wins X grid = putStrLn "Player X wins!\n"
  | full grid = putStrLn "It's a draw!\n"
  | player == O = do
      i <- getNat (prompt player)
      case move grid i player of
        [] -> do
          putStrLn "ERROR: Invalid move"
          play' player tree
        [g'] -> do
          let subtree = head [t | t@(Node (g, _) _) <- ts, g /= grid, g == g']
          play (next player) subtree
  | player == X = do
      putStr "Player X is thinking... "
      {-
        WARN: incompatible with other exercises
        INFO: Exercise 11.2:

        let gs = bestmoves g p
        n <- randomRIO (0, length gs - 1)
        play (gs !! n) (next p)
      -}
      {-
        WARN: incompatible with other exercises
        INFO: Exercise 11.3:

        let g' = shortestmove g p
        play g' (next p)
      -}
      let subtree = head [t | t@(Node (_, p) _) <- ts, p == bestPlayer]
      play (next player) subtree
