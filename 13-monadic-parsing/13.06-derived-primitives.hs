import           Control.Applicative
import           Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> []
    (x:xs) -> [(x, xs)]

-- | Three basic parsers: `item`, `return v`, and `empty`.

-- | Succeeds for Single characters that satify the predicate function `p`.
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
  then return x
  else empty

-- | The `digit` parser is defined for single digits.
digit :: Parser Char
digit = sat isDigit

-- | The `lower` parser is defined for lower-case letters.
lower :: Parser Char
lower = sat isLower

-- | The `upper` parser is defined for upper-case letters.
upper :: Parser Char
upper = sat isUpper

-- | The `letter` parser is defined for arbitrary letters.
letter :: Parser Char
letter = sat isAlpha

-- | The `alphaum` parser is defined for alphanumerica characters.
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- | The `char` parser is defined for specific characters.
char :: Char -> Parser Char
char x = sat (== x)

-- | The `string` parser is defined for the string of characters `xs`
-- with the string itself returned as the result value and succeeds only if the
-- entire target string is consumed from the input to the parser.
string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- | The `ident` parser is defined for strings comprising a lower-case letter
-- followed by zero or more alphanueric characters
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

-- | The `nat` parser is defined for natural numbers comprising one or more
-- digits.
nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- | The `space` parser is defined for spacing comprising zero or more space,
-- tab, and newline characters.
space :: Parser ()
space = do
  many (sat isSpace)
  return ()

-- | The `int` parser is defined for integer values.
int :: Parser Int
int = do
  char '-'
  n <- nat
  return (-n) <|> nat

instance Functor Parser where
  fmap f p = P $ \inp ->
    case parse p inp of
      []         -> []
      [(v, out)] -> [(f v, out)]

instance Applicative Parser where
  pure v = P $ \inp -> [(v, inp)]

  pf <*> px = P $ \inp ->
    case parse pf inp of
      []         -> []
      [(f, out)] -> parse (fmap f px) out

instance Monad Parser where
  p >>= f = P $ \inp ->
    case parse p inp of
      []         -> []
      [(v, out)] -> parse (f v) out

instance Alternative Parser where
  empty = P $ \inp -> []

  p <|> q = P $ \inp ->
    case parse p inp of
      []         -> parse q inp
      [(v, out)] -> [(v, out)]
