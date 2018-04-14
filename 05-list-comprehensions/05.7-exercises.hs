-- Exercises 5.7

-- 1.
sumOfHundredSquareIntegers = sum [x^2 | x <- [1..100]]

-- 2.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3.
square' :: Int -> [(Int, Int)]
square' n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [0..n]]

-- 5.
pyths :: Int -> [(Int, Int ,Int)]
pyths n = [(x, y, z) | x <- [1..n],
                       y <- [1..n],
                       z <- [1..n],
                       (x^2) + (y^2) == (z^2)]

-- 6.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- 7.
-- Show how the list comprehension [(x, y) | x <- [1, 2], y <- [3, 4]] with two generators can be
-- expressed using two comprehensions with a single generator

nestedComprehension xs ys = concat [[(x, y) | y <- ys] | x <- xs]

-- 8.
-- Redefine the function `positions` using the function `find`

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

findPositions :: Eq a => a -> [a] -> [Int]
findPositions x xs = find x (zip xs [0..])

-- 9.
-- The scalar product of two lists of integers `xs` and `ys` of length `n` is given
-- by the sum of the products of cooresponding integers.

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
