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
