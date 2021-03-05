merge0 :: Ord a => [a] -> [a] -> [a]
merge0 [] ys = ys
merge0 xs [] = xs
merge0 (x:xs) (y:ys) | x < y = x : merge0 xs (y:ys)
                     | otherwise = y : merge0 (x:xs) ys


halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take halfLength xs, drop halfLength xs)
            where halfLength = length xs `div` 2



msort0 :: Ord a => [a] -> [a]
msort0 [] = []
msort0 [x] = [x]
msort0 xs = merge0 (msort0 left) (msort0 right)
            where
              halves = halve xs
              left = fst  halves
              right = snd halves

