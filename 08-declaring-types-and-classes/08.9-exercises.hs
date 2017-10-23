-- 1.
-- Define a recursive multiplication function for the recursive type of natural
-- numbers.
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _     = Zero
mult _ Zero     = Zero
mult (Succ m) n = add (Succ m) (mult m n)

-- 2.
-- Together with `compare`, define the function `occurs` for search trees.
-- compare :: Ord a => a -> a -> Ordering
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                 = x == y
occurs x (Node l y r) | x == y    = True
                      | x < y     = occurs x l
                      | otherwise = occurs x r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)     = x == y 
occurs' x (Node l y r) = case compare x y of 
                           LT -> occurs' x l
                           EQ -> True
                           GT -> occurs' x r
 
-- 3.
-- Define a function that decides if the number of leaves in
-- the left and right subtree of every node differs by at
-- at most one, with leaves themselves being trivially
-- balanced.
data BTree a = BLeaf a | BNode (BTree a) (BTree a) deriving Show 

leafLen :: BTree a -> Int 
leafLen (BLeaf x)   = 1
leafLen (BNode l r) = leafLen l + leafLen r

balanced :: BTree a -> Bool
balanced (BLeaf x)   = True
balanced (BNode l r) = 
  abs (leafLen l - leafLen r) <= 1 && balanced l && balanced r

-- 4.
-- Define a function that converts a non-empty list into a
-- balanced tree.
splitList :: [a] -> ([a], [a])
splitList xs = ((take n xs), (drop n xs))
  where
    n = length xs `div` 2

balance :: [a] -> BTree a
balance [x] = BLeaf x
balance xs  =
  BNode 
    (balance (fst (splitList xs)))
    (balance (snd (splitList xs)))

-- 5.
-- Define a higher-order function such that replaces each
-- `Val` constructor in an expression by the function `f`,
-- and each `Add` constructor by the function `g`.
data Expr' = Val' Int | Add' Expr' Expr' deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr' -> a
folde f g (Val' x)   = f x
folde f g (Add' x y) = g (folde f g x) (folde f g y)

-- 6.
-- Using `folde` define a function `eval` that evaluates an
-- expression to an integer value, and a function `size`
-- that calculates the number of values in an expression.
eval' :: Expr' -> Int 
eval' = folde (+0) (+)

size :: Expr' -> Int
size = folde (\_ -> 1) (+)

-- 7.
-- Complete the following instance declarations:
--
-- instance Eq a => Eq (Maybe a) where
--   (==) Nothing Nothing   = True
--   (==) (Just x) (Just y) = x == y
--   (==) _ _               = False
--
-- instance Eq a => Eq [a] where
--   (==) [] []         = True
--   (==) (x:xs) (y:ys) = x == y && xs == ys
--   (==) _ _           = False
--

-- 8.
-- Extend the tautology checker to support logical 
-- disjunction and equivalence in propositions.
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          -- extension for ∨ and ⇔ 
          | Or Prop Prop
          | Eqiv Prop Prop
          deriving Show

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
-- extension for ∨ and ⇔ 
eval s (Or p q)    = eval s p || eval s q
eval s (Eqiv p q)  = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
-- extension for ∨ and ⇔ 
vars (Or p q)    = vars p ++ vars q
vars (Eqiv p q)  = vars p ++ vars q

-- 9.
-- Extend the abstract machine to support the use
-- of multiplication.
data Expr = 
    Val Int 
  | Add Expr Expr
  | Mult Expr Expr

data Op =
    ADD Expr
  | MULT Expr
  | PLUS Int
  | TIMES Int

type Cont = [Op]

eval'' :: Expr -> Cont -> Int
eval'' (Val n) c    = exec c n
eval'' (Add x y) c  = eval'' x (ADD y : c)
eval'' (Mult x y) c = eval'' x (MULT y : c)

exec :: Cont -> Int -> Int
exec [] n            = n
exec (ADD y : c) n   = eval'' y (PLUS n : c)
exec (MULT y : c) n  = eval'' y (TIMES n : c)
exec (PLUS n : c)  m = exec c (n + m)
exec (TIMES n : c) m = exec c (n * m)

value :: Expr -> Int
value e = eval'' e []
