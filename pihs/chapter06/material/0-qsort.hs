







qsort0 :: Ord a => [a] -> [a]
qsort0 []     = []
qsort0 (x:xs) = qsort0 smaller ++ [x] ++ qsort0 larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]