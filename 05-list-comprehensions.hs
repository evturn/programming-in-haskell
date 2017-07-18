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
