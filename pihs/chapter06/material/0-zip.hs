


zip0 :: [a] -> [b] -> [(a, b)]
zip0 []     _      = []
zip0 _      []     = []
zip0 (x:xs) (y:ys) = (x,y) : zip0 xs ys
