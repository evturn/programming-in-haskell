type State = Int

newtype ST a = S (State -> (a, State))

apState :: ST a -> State -> (a, State)
apState (S st) x = st x

mkState :: a -> ST a
mkState x = S (\y -> (x, y))

instance Functor ST where
  fmap f (S st) = S $ \s ->
    let (x, s') = st s
     in (f x, s')

instance Applicative ST where
  pure x = S $ \s -> (x, s)
  (S fx) <*> (S gx) = S $ \s ->
    let (f, s')  = fx s
        (x, s'') = gx s'
     in (f x, s'')

instance Monad ST where
  (S st) >>= f = S $ \s ->
    let (x, s') = st s
        (S st') = f x
     in st' s'

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n   = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n')  = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

mlabel' :: Tree a -> ST (Tree Int)
mlabel' (Leaf _)   = fresh >>= \n ->
                     return (Leaf n)
mlabel' (Node l r) = mlabel' l >>= \l' ->
                     mlabel' r >>= \r' ->
                     return (Node l' r')


