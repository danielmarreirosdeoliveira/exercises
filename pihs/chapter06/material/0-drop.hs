



-- drop0 :: Integral b => b -> [a] -> [a]
drop0 :: Int -> [a] -> [a]
drop0 0 xs = xs
drop0 _ [] = []
drop0 n (_:xs) = drop0 (n-1) xs