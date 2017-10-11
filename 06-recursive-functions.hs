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
