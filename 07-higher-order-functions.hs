import Prelude hiding (all, any, takeWhile)
-- 1.
-- Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and filter.

f1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
f1 p f xs = map f $ filter p xs

-- 2.
-- Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.
-- a.
-- Decide if all elements of a list satisfy a predicate:
all :: (Int -> Bool) -> [Int] -> Bool
all f = foldr (\x y -> f x && y) True 

-- b.
-- Decide if any element of a list satisfies a predicate:
any :: (Int -> Bool) -> [Int] -> Bool
any f = foldr (\x y -> f x || y) False
  
-- c.
-- Select elements from a list while they satisfy a predicate:
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs)
  | f x       = x : takeWhile f xs
  | otherwise = []
