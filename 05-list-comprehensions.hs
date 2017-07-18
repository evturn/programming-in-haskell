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
perfects n = [sum (factors x) | x <- [1..n]]
