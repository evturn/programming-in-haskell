-- Exercises 6.8

-- 1.
factorial :: Int -> Int
factorial 0         = 1
factorial n | n > 0 = n * factorial (n - 1)

-- 2.
sumdown :: Int -> Int
sumdown 0         = 0
sumdown n | n > 0 = n + sumdown (n - 1)
