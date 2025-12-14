-- Exercise 13.8: Consider expressions built up from natural numbers using a
-- subtraction operator that is assumed to associate to the left.
--
-- a. Translate this description directly into a grammar.
-- b. Implement this grammar as a parser expr :: Parser Int.
-- c. What is the problem with this parser?
-- d. Show how it can be fixed. Hint: rewrite the parser using the repetition
--    primitive many and the library function foldl.

-- a. Translate this description directly into a grammar.

{-
expr ::= (expr -) nat
nat ::= 0 | 1 | 2 | ...
-}

-- b. Implement this grammar as a parser expr :: Parser Int.

import Control.Applicative
import Parser

expr :: Parser Int
expr = go <*> natural
  where
    -- returns empty if parsing an expr fails
    -- otherwise returns left associative minus
    go = do
      e <- expr
      symbol "-"
      return (e -)

-- c. What is the problem with this parser?

{-
The parser is infinitely recursive in that it does not
know when to stop parsing an expr.
-}

-- d. Show how it can be fixed. Hint: rewrite the parser using the repetition
--    primitive many and the library function foldl.

-- many :: Alternative f => f a -> f [a]
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- parse many (natural -)
-- then foldl over the parsed naturals minus the last natural

-- foldl1 takes the first 2 items of the list and applies the function to them,
-- then feeds the function with this result and the third argument and so on

expr' :: Parser Int
expr' = do
  ns <- many nats
  n <- natural
  return $ foldl1 (-) ns - n
  where
    nats = do
      n <- natural
      symbol "-"
      return n

eval :: String -> Int
eval xs = case parse expr' xs of
  [(e, [])] -> e
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "invalid input"
