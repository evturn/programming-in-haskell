import           Control.Applicative
import           Data.Char

newtype Parser a = P (String -> [(a, String)])

-- | Removes the `P` dummy constructor from the `Parser` type and applies it to
-- an input string.
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- | Fails if the input string is empty, otherwise succeeds with the first
-- character as the result value.
item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> []
    (x:xs) -> [(x, xs)]
