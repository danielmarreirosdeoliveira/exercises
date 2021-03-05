fac0 :: Int -> Int
fac0 n = product[1..n]

fac1 :: Int -> Int
fac1 0 = 1
fac1 n = n * fac1 (n-1)