-- 1.
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

instance Functor Tree where
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
  fmap _ Leaf         = Leaf

-----------------------------------------------------------------------------
-- 2.
instance Functor ((->) a) where
  fmap = (.)

-----------------------------------------------------------------------------
-- 3.
instance Applicative ((->) a) where
  pure x = \_ -> x
  f <*> g = \x -> f x $ g x

-----------------------------------------------------------------------------
-- 4.
newtype ZipList a = Z [a]
  deriving Show

instance Functor ZipList where
  fmap f (Z xs) = Z (fmap f xs)

instance Applicative ZipList where
  pure x = Z $ repeat x
  Z fs <*> Z xs = Z [f x | (f, x) <- zip fs xs]

-----------------------------------------------------------------------------
-- 5.
-- What?

-----------------------------------------------------------------------------
-- 6.
instance Monad ((->) a) where
  return = pure
  mx >>= f = \x -> f (mx x) x

-----------------------------------------------------------------------------
-- 7.
data Expr a = Var a
            | Val Int
            | Add (Expr a) (Expr a)
            deriving Show

instance Functor Expr where
  fmap f (Var x)   = Var (f x)
  fmap _ (Val x)   = Val x
  fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Applicative Expr where
  pure = Var
  _       <*> Val x   = Val x
  Val x   <*> _       = Val x
  Var f   <*> Var x   = Var (f x)
  Var f   <*> Add x y = Add (fmap f x) (fmap f y)
  Add f g <*> x       = Add (f <*> x) (g <*> x)

instance Monad Expr where
  return = pure
  Val x   >>= _ = Val x
  Var x   >>= f = f x
  Add x y >>= f = Add (x >>= f) (y >>= f)

-----------------------------------------------------------------------------
-- 8.
type State = Int

newtype ST a = S (State -> (a, State))

instance Functor ST where
  fmap f st = do
    x <- st
    S $ \s -> (f x, s)

instance Applicative ST where
  pure x = S $ \s -> (x, s)
  stf <*> stx = do
    f <- stf
    x <- stx
    return $ f x

instance Monad ST where
  S st >>= f = S $ \s ->
    let (x, s') = st s
        (S st') = f x
     in st' s'
