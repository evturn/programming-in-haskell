-- Exercise 14.5

-- 1.
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (x1, y1) <> (x2, y2) = (x1 <> x2, y1 <> y2)

-- 2.
instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty
  f <> g = \x -> f x <> g x 

-- 3.
instance Traversable Maybe where
  foldr _ _ Nothing = Nothing
  foldr f x (Just y) = Just (f (x `mappend` y))
