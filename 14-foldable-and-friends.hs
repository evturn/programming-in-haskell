-- Exercise 14.5

-- 1.
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (x1, y1) <> (x2, y2) = (x1 <> x2, y1 <> y2)
