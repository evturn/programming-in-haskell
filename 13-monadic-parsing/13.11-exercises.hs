import           Control.Applicative
import           Data.Char

-----------------------------------------------------------------------------
-- 1.
comment :: Parser ()
comment = do
  string "--"
  many $ sat (/= '\n')
  return ()

-----------------------------------------------------------------------------
-- 2.
--          expr
--          / | \
--         /  +  \
--       expr    expr
--      / | \       \
--     /  +  \       \
--   expr    expr    term
--    |        |       |
--   term    term   factor
--    |        |       |
--  factor  factor    nat
--    |        |       |
--   nat      nat      4
--    |        |
--    2        3

-----------------------------------------------------------------------------
-- 3.
--        expr
--        / | \
--       /  +  \
--     term    expr
--       |       |
--    factor   term
--       |       |
--      nat   factor
--       |       |
--       2      nat
--               |
--               3

-----------------------------------------------------------------------------
-- 4.
-- Answer: Each number would end up being parsed multiple times before
-- recognizing that it could be an expression.

-----------------------------------------------------------------------------
-- 5.
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          deriving Show

expr' :: Parser Expr
expr' = do
  t <- term
  do symbol "+"
     e <- expr'
     return (Add (Val t) e)
   <|> do symbol "-"
          e <- expr'
          return (Sub (Val t) e)
   <|> return (Val t)

-----------------------------------------------------------------------------

expr :: Parser Int
expr = do
  t <- term
  do symbol "+"
     e <- expr
     return (t + e)
   <|> do symbol "-"
          e <- expr
          return (t - e)
   <|> return t

term :: Parser Int
term = do
  f <- factor
  do symbol "*"
     t <- term
     return (f * t)
   <|> do symbol "/"
          t <- term
          return (f `div` t)
   <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> nat
-----------------------------------------------------------------------------
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> []
    (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
  then return x
  else empty

symbol :: String -> Parser String
symbol xs = token (string xs)

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

digit :: Parser Char
digit = sat isDigit

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

char :: Char -> Parser Char
char x = sat (== x)

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
