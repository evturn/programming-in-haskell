-- Exercises 6.8

-- 1.
factorial :: Int -> Int
factorial 0         = 1
factorial n | n > 0 = n * factorial (n - 1)

-- 2.
sumdown :: Int -> Int
sumdown 0         = 0
sumdown n | n > 0 = n + sumdown (n - 1)

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
