import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, iterate)
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

-- d.
-- Remove elements from a list wile they satisfy a predicate:
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs)
  | f x       = dropWhile f xs
  | otherwise = x : xs

-- 3.
-- Redefine the functions map f and filter p using foldr.
map f = foldr (\x y -> f x : y) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x y -> if p x then x : y else y) [] 

-- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer. For example:
-- dec2int [2,3,4,5]
-- 2345
--
dec2int :: [Int] -> Int
dec2int = foldl (\ys x -> ys * 10 + x) 0 

-- 5. Define the higher-order library function curry that converts a function on pairs into a curried function, and, conversely, the function uncurry that converts a curried function with two arguments into a function on pairs.
currry :: ((a, b) -> c) -> (a -> b -> c)
currry f = \x y -> f (x, y)

uncurrry :: (a -> b -> c) -> ((a, b) -> c)
uncurrry f = \(x, y) -> f x y

-- 6.
-- Redefine the functions chop8, map f, and iterate f using unfold:
-- unfold p h t x 
--  | p x       = []
--  | otherwise = h x : unfold p h t (t x)
chop8 :: [Int] -> [[Int]] 
chop8 [] = []
chop8 xs = take 8 xs : chop8 (drop 8 xs)

iterate :: (a -> a) -> a -> [a]
iterate f n = n : iterate f (f n) 
