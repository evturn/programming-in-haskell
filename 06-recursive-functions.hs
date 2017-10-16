-- Exercises 6.8

-- 1.
-- Modify the factorial function to prohibit negative arguments by adding a guard 
-- to the recursive case.
factorial :: Int -> Int
factorial n
  | n < 0  = 0
  | n == 0 = 1
  | n > 0  = n * factorial (n - 1)

-- 2.
-- Define a recursive function that returns the sum of non-negative numbers from a
-- given value down to zero.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3.
-- Define the exponentiation operator `^` for non-negative integers using the same
-- pattern of recursion as the multiplication operator `*`.
(<^>) :: Int -> Int -> Int
0 <^> _ = 0
n <^> 0 = 1
n <^> 1 = n 
n <^> m = n * (n <^> (m - 1)) 

-- 4.
-- Euclid's algorithm is used for calculating the greatest common divisor of two
-- non-negative integers
euclid :: Int -> Int -> Int
euclid n 0 = n
euclid n m = euclid m (n `mod` m) 

-- 5.
-- Show how `length [1,2,3]`, `drop 3 [1,2,3,4,5]`, and `init [1,2,3]` are evaluated
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs
-- length' [1,2,3]
-- 1 + (length' [2,3])
-- 1 + 1 + (length' [3])
-- 1 + 1 + 1 + (length' [])
-- 1 + 1 + 1 + 0

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n - 1) xs
-- drop' 3 [1,2,3,4.5]
-- drop' 2 [2,3,4,5]
-- drop' 1 [3,4,5]
-- drop' 0 [4,5]
-- [4,5]

init' :: [a] -> [a]
init' [_]    = []
init' (x:xs) = x : init' xs
-- init' [1,2,3]
-- 1 : init' [2,3]
-- 1 : 2 : init' [3]
-- 1 : 2 : []
-- [1,2]

-- 6.
-- Define the following library functions on lists using recursion.
-- a.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = 
  if x == False
     then False 
  else and' xs 

-- b.
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- c.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- d.
(<!!>) :: [a] -> Int -> a
(<!!>) (x:xs) 0 = x
(<!!>) (x:xs) n = (<!!>) xs (n-1)

-- e.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = 
  if a == x
     then True
  else elem' a xs

-- 7.
-- Define a recursive function hat merges two lists to give a single sorted list.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
  | x < y     = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- 8.
-- Define a function using merge that implements merge sort, in which the empty
-- list and singleton lists are already sorted, and any other list is sorted by
-- merging together the two lists that result from sorting the two halves.
halve :: [a] -> ([a], [a])
halve xs = ((take n xs), (drop n xs))
  where 
    n = length xs `div` 2 
    
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x] 
msort xs = merge (msort first) (msort second)
  where 
    (first, second) = halve xs

-- 9.
-- Construct the library functions that:
-- a. calculate the sum of a list of numbers;
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

-- b. take a given number of elements from the start of a list;
take' :: Int -> [a] -> [a] 
take' 0 _  = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs 
  
-- c. select the last element of a non-empty list.
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
