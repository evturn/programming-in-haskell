import           Control.Applicative

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> []
    (x:xs) -> [(x, xs)]

instance Functor Parser where
  fmap f p = P $ \str ->
    case parse p str of
      []        -> []
      [(x, xs)] -> [(f x, xs)]

instance Applicative Parser where
  pure x = P $ \str -> [(x, str)]

  pf <*> px = P $ \str ->
    case parse pf str of
      []        -> []
      [(f, xs)] -> parse (fmap f px) xs

instance Monad Parser where
  return = pure

  p >>= f = P $ \str ->
    case parse p str of
      []        -> []
      [(x, xs)] -> parse (f x) xs

instance Alternative Parser where
  empty = P $ \x -> []

  p <|> q = P $ \x ->
    case parse p x of
      []       -> parse q x
      [(v, y)] -> [(v, y)]
