-- Exercise 13.9: Modify the calculator program to indicate the approximate
-- position of an error rather than just sounding a beep, by using the fact that
-- the parser returns the unconsumed part of the input string.

import Control.Applicative
import Parser
import Screen

-- Store the approximate position of the error
-- and set the background color to red.
data Error = Err Int | None

box :: [String]
box =
  [ "+---------------+",
    "|               |",
    "+---+---+---+---+",
    "| q | c | d | = |",
    "+---+---+---+---+",
    "| 1 | 2 | 3 | + |",
    "+---+---+---+---+",
    "| 4 | 5 | 9 | - |",
    "+---+---+---+---+",
    "| 7 | 8 | 9 | * |",
    "+---+---+---+---+",
    "| 0 | ( | ) | / |",
    "+---+---+---+---+"
  ]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*()/"
    extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1 ..] box]

display :: String -> Error -> IO ()
display xs (Err n) = do
  writeat (3, 2) (replicate 13 ' ')
  writeat (3, 2) (take n s ++ bgred ++ [s !! n] ++ bgclear ++ drop (n + 1) s)
  where
    s = reverse (take 13 (reverse xs))
display xs None = do
  writeat (3, 2) (replicate 13 ' ')
  writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> Error -> IO ()
calc xs e = do
  display xs e
  c <- getCh
  if c `elem` buttons
    then
      process c xs
    else do
      beep
      calc xs e

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs
  | c `elem` "qQ\ESC" = quit
  | c `elem` "dD\BS\DEL" = delete xs
  | c `elem` "=\n" = eval xs
  | c `elem` "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc [] None
delete xs = calc (init xs) None

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n, [])] -> calc (show n) None
  [(_, out)] -> do
    beep
    calc xs (Err $ length xs - length out)
  [] -> do
    beep
    calc xs (Err 0)

clear :: IO ()
clear = calc [] None

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c]) None

run :: IO ()
run = do
  cls
  showbox
  clear

-- parser for expressions

expr :: Parser Int
expr =
  do
    t <- term
    do
      symbol "+"
      e <- expr
      return (t + e)
      <|> do
        symbol "-"
        e <- expr
        return (t - e)
      <|> return t

term :: Parser Int
term =
  do
    f <- factor
    do
      symbol "*"
      t <- term
      return (f * t)
      <|> do
        symbol "/"
        t <- term
        return (f `div` t)
      <|> return f

factor :: Parser Int
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> natural
