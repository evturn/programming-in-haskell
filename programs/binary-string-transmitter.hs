import Data.Char

type Bit = Int
type Bits = [Bit]

bin2int :: Bits -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- bin2int [1,0,1,1]
-- 13

int2bin :: Int -> Bits
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- int2bin 13
-- [1,0,1,1]

make8 :: Bits -> Bits
make8 bits = take 8 (bits ++ repeat 0)

-- make8 [1,0,1,1]
-- [1,0,1,1,0,0,0]

encode :: String -> Bits
encode = concat . map (make8 . int2bin . ord)

-- encode "sup"
-- [1,1,0,0,1,1,1,0,1,0,1,0,1,1,1,0,0,0,0,0,1,1,1,0]

chop8 :: Bits -> [Bits]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: Bits -> String
decode = map (chr . bin2int) . chop8

-- decode [1,1,0,0,1,1,1,0,1,0,1,0,1,1,1,0,0,0,0,0,1,1,1,0]
-- "sup"

channel :: Bits -> Bits
channel = id

transmit :: String -> String
transmit = decode . channel . encode
