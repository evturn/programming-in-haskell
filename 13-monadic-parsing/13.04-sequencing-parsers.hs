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

-- | Applies a function to the result value of a parser if the parser succeeds,
-- and propagates the failure otherwise.
instance Functor Parser where
  fmap f p = P $ \str ->
    case parse p str of
      []        -> []
      [(x, xs)] -> [(f x, xs)]

-- | The `pure` function transforms a value into a parser that always succeeds
-- with this value as its result without consuming any of the input string.
-- | The `<*>` function applies a parser `pf` that returns a function `f` and
-- an argument `xs` to a parser `px` that returns the result of applying
-- the function `fmap f px` to the argument `xs`, and only succeeds if all the
-- components succeed.
instance Applicative Parser where
  pure x = P $ \str -> [(x, str)]

  pf <*> px = P $ \str ->
    case parse pf str of
      []        -> []
      [(f, xs)] -> parse (fmap f px) xs

-- | Applies the function `f` to the result value `x` from applying the parser
-- `p` to the input string `str`, which when `f x` is applied returns a parser
-- which is then applied with the output string `xs`.
instance Monad Parser where
  return = pure

  p >>= f = P $ \str ->
    case parse p str of
      []        -> []
      [(x, xs)] -> parse (f x) xs
