import Data.Char

type Bit = Int

bin2int0 :: [Bit] -> Int
bin2int0 bits = sum [w * b | (w,b) <- zip weights bits ]
                where
                  weights = iterate (*2) 1

bin2int1 :: [Bit] -> Int
bin2int1 = foldr (\x y -> x + 2*y) 0

int2bin0 :: Int -> [Bit]
int2bin0 0 = []
int2bin0 n = n `mod` 2 : int2bin0 (n `div` 2)

make8 bits = take 8 (bits ++ repeat 0)

encode0 :: String -> [Bit]
encode0 = concat . map (make8 . int2bin0 . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode0 :: [Bit] -> String
decode0 = map (chr . bin2int1) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode0 . channel . encode0

-- *Main> transmit "higher-order functions are easy"
-- "higher-order functions are easy"
