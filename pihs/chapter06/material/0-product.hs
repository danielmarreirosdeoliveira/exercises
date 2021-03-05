


product0 :: Num a => [a] -> a
product0 [] = 0
product0 (n:ns) = n * product ns


product1 :: Num a => [a] -> a
product1 = foldr (*) 1

product2 :: Num a => [a] -> a
product2 = foldl (*) 1