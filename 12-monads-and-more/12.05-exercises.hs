-- 1.
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

instance Functor Tree where
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
  fmap _ Leaf         = Leaf

-----------------------------------------------------------------------------
-- 2.
--
-- |
-- Avoid conflict with library definition for `((->) r)`
newtype R r a = R
              { run :: r -> a }

instance Functor (R r) where
  fmap f  (R r) = R $ f . r

applyR :: (Int -> Int) -> R Int Int -> Int -> Int
applyR f r x = run (fmap f r) x

runR :: Int
runR = applyR (+100) (R $ (+2)) 3
