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

three :: Parser (Char, Char)
three = pure f <*> item <*> item <*> item
  where
    f x y z = (x, z)

instance Monad Parser where
  p >>= f = P $ \inp ->
    case parse p inp of
      []         -> []
      [(v, out)] -> parse (f v) out

three' :: Parser (Char, Char)
three' = do
  x <- item
  item
  z <- item
  return (x, z)
