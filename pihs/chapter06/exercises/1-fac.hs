


fac0 :: Int -> Int
fac0 0 = 1
fac0 n | n > 0     = n * fac0 (n-1)
--        | otherwise = n                 we could return 0, -1 or n as an alternative to an error