-- Exercise 13.5: Define a suitable type Expr for arithmetic expressions and
-- modify the parser for expressions to have type expr :: Parser Expr.

-- Exercise 13.6: Extend the parser expr :: Parser Int to support subtraction
-- and division, and to use integer vlaues rather than natural numbers, based
-- upon the following revision to the grammar.

-- Exercise 13.7: Further extend the grammar and parser for arithmetic
-- expressions to support exponentiation ^, which is assumed to associate to the
-- right and have higher priority than multiplication and division, but lower
-- priority than parentheses and numbers. For example, 2^3*4 means (2^3)*4.

import Control.Applicative
import Parser

{-
expr ::= term (+ expr | - expr | Є)
term ::= power (* term | / term | Є)
power ::= factor (^ power | Є)
factor ::= ( expr ) | int
int ::= ... | -1 | 0 | 1 | ...
-}

data Expr
  = Val Int -- value
  | Add Expr Expr -- addition
  | Sub Expr Expr -- subtraction
  | Mul Expr Expr -- multiplication
  | Div Expr Expr -- division
  | Pow Expr Expr -- exponentiation
  | Par Expr -- parenthesized expression
  deriving (Show)

expr :: Parser Expr
expr =
  do
    t <- term
    do
      symbol "+"
      Add t <$> expr
      <|> do
        symbol "-"
        Sub t <$> expr
      <|> do
        return t

term :: Parser Expr
term =
  do
    p <- power
    do
      symbol "*"
      Mul p <$> term
      <|> do
        symbol "/"
        Div p <$> term
      <|> do
        return p

power :: Parser Expr
power =
  do
    f <- factor
    do
      symbol "^"
      Pow f <$> term
      <|> return f

factor :: Parser Expr
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return (Par e)
    <|> integer'

-- parser for integers
int' :: Parser Expr
int' =
  do
    char '-'
    n <- nat
    Val . negate <$> nat
    <|> Val <$> nat

-- parse integer
integer' :: Parser Expr
integer' = token int'

eval :: String -> Expr
eval xs = case parse expr xs of
  [(e, [])] -> e
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "invalid input"
