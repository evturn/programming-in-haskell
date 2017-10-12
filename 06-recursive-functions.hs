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
