merge0 :: Ord a => [a] -> [a] -> [a]
merge0 [] ys = ys
merge0 xs [] = xs
merge0 (x:xs) (y:ys) | x < y = x : merge0 xs (y:ys)
                     | otherwise = y : merge0 (x:xs) ys