


sum0 :: Num a => [a] -> a
sum0 [] = 0
sum0 (x:xs) = x + sum0 xs

take0 :: Int -> [a] -> [a]
take0 0 _ = []
take0 n [] = []
take0 n (x:xs) = x : take0 (n-1) xs

last0 :: [a] -> a
last0 [x] = x
-- last0 (x:[]) = x
last0 (_:xs) = last0 xs