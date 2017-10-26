-- 1.
-- Redefine the combinatorial function `choices` using a list comprehension.
choices' :: [a] -> [[a]]
choices' xs = [ys | yss <- subs xs,
                     ys <- perms yss]

-- The coundown problem
--
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
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x  `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)      = show n
  show (App o l r)  = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- | This returns all subsequences of a list, which are given by all possible
-- combinations of excluding or including each element of the list.
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where
    yss = subs xs

-- | This returns all possible ways of inserting a new element into a list.
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- | This returns all permutations of a list, which are given by all possible
-- reorderings of the elements.
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- | This returns all choices from a list, which are given by all possible
-- ways of selecting zero or more elemenets in any order by considering all
-- permutations of all subsequences.
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- | This defines what it means to solve an instance of the countdown 
-- problem.
--
-- Given a list of numbers and a target, an expression is a solution if:
-- 
-- * The list of values in the expression is chosen from the list of numbers.
-- * The expression successfully evaluates to give the target.
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

-- | Returns all possible ways of splitting a list into two non-empty lists
-- that append to give the original list.
split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

-- | Returns all possible expressions whose list of values is precisely a
-- given list.
-- For an empty list of numbers there are no possible expressions.
-- For a single number there is a single expression comprising that number.
-- Otherwise, for a list of two or more numbers first produce all splittings
-- of the list, then recursively calculate all possible expressions for each
-- of these lists.
-- Finally, combine each pair of expressions using each of the four numeric
-- operators.
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l        <- exprs ls,
                r        <- exprs rs,
                e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- | Returns all possible expressions that solve an instance of the countdown
-- problem by first generating all expressions over each choice from the given
-- list of numbers, and selecting those expressions that successfully evaluate
-- the target.
solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
  
main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)

type Result = (Expr, Int)

-- | Returns all possible results comprising expressions whose list of values
-- is precisely a given list.
--
-- For the empty list there are no possible results.
-- For a single number there is a single result formed from that number.
-- Otherwise, for two or more numbers we first produce all splittings of the
-- list, then recursively calculate all possible results for each of these
-- lists
-- Finally, combine each of the four numeric operators that are valid.
results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                      lx      <- results ls,
                      ry      <- results rs,
                      res     <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e, m) <- results ns', m == n]
