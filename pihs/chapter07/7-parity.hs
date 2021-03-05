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
encode0 = concat . map (addParity . make8 . int2bin0 . ord)

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : chop n (drop n bits)

chop9 :: [Bit] -> [[Bit]]
chop9 = chop 9

decode0 :: [Bit] -> String
decode0 = map (chr . bin2int1 . dropParity . testParity) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode0 . channel . encode0

--
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

parity :: [Bit] -> Bit
parity bits = if odd (count 1 bits) then 1 else 0

addParity :: [Bit] -> [Bit]
addParity bits = [parity bits] ++ bits

testParity :: [Bit] -> [Bit]
testParity bits = if parity (drop 1 bits) == head bits then bits else error "Parity check failed"

dropParity = drop 1


-- *Main> transmit "higher-order functions are easy"
-- "higher-order functions are easy"
