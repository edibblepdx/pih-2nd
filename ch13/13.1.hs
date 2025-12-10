-- Exercise: Define a parser comment :: Parer () for ordinary Haskell comments
-- that begin with the symbol -- and extend to the end of the current line, which
-- is represented by the control character `\n`.

import Control.Applicative
import Parser

comment :: Parser ()
comment = do
  string "--"
  many (sat (/= '\n'))
  return ()
