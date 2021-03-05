


filter0 :: (a -> Bool) -> [a] -> [a]
filter0 p xs = [x | x <- xs, p x]

-- *Main> filter0 (>2) [2,1,7,0]
-- [7]
-- *Main> filter0 even [2,1,7,0]
-- [2,0]
-- *Main> filter0 (/= ' ') "abc def ghi"
-- "abcdefghi"

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs) | p x     = x : filter1 p xs
                 | otherwise = filter1 p xs