

insert0 :: Ord a => a -> [a] -> [a]
insert0 x [] = [x]
insert0 x (y:ys) | x <= y    = x : y : ys
                 | otherwise = y : insert0 x ys



isort0 :: Ord a => [a] -> [a]
isort0 []     = []
isort0 (x:xs) = insert0 x (isort0 xs)

