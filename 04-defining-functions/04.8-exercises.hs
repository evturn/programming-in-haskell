-- Exercises 4.8

-- 1.
firstHalf :: [a] -> [a]
firstHalf xs = (take $ (length xs) `div` 2) xs

secondHalf :: [a] -> [a]
secondHalf xs = (drop $ (length xs) `div` 2) xs

halve :: [a] -> ([a], [a])
halve xs = (firstHalf xs, secondHalf xs)

-- 2.
-- a.
thirdA :: [a] -> a
thirdA xs = head $ tail $ tail xs

-- b.
thirdB :: [a] -> a
thirdB xs = xs !! 2

-- c.
thirdC :: [a] -> a
thirdC (_:_:z:xs) = z

-- 3.
-- a.
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

-- b.
safetailB :: [a] -> [a]
safetailB xs | null xs   = []
             | otherwise = tail xs
-- c.
safetailC :: [a] -> [a]
safetailC [] = []
safetailC xs = tail xs

-- 8.
-- Luhn algorithm

luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = (x * 2) - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (((luhnDouble w) + x + (luhnDouble y) + z) `mod` 10) == 0

