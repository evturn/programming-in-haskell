-- Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and filter.

f1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
f1 p f xs = map f $ filter p xs
