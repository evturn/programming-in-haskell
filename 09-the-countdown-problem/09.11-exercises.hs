-- Given a sequence of numbers and a target number, attempt to construct an
-- expression whose value is the target, by combining one or more numbers from
-- the sequence using addition, subtraction, multiplication, division and
-- parentheses. Each number in the sequence can only be used at most once and
-- all of the involved, including intermediate values must be positive natural
-- numbers.

data Op =
    Add
  | Sub
  | Mul
  | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
