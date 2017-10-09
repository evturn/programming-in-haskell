import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- a Frequency table by analyzing a large volume of such text, one can derive the
-- following percentage frequencies of the twenty-six letters of the alphabet.
table :: [Float]
table = [8.1, 1.5, 2.8, 4.5, 12.7, 2.2, 2.0, 6.1, 7.0, 
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- using `percent` within a list comprehension, together with `lowers` and `count`
-- a function can be defined that returns a frequency table for any given string.
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where
             n = lowers xs

-- the chi-square statistic compares a list of observed frequencies with a
-- list of expected frequencies. 
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

-- a function that rotates the elements of a list n places to the left, wrapping
-- around at the start of the list (assuming that n is between zero and the length
-- of the list.
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- a function that can decode most strings produced using the Caesar cipher by
-- producing a frequency table of the encoded string, calculating the chi-square
-- statistic for each possible rotation of this table alongside the table of
-- expected frequencies, and using the position of the minimum chi-square value
-- as the shift factor to then decode the string.
crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
