dec2int :: [Int] -> Int
dec2int = foldl (\acc val -> acc * 10 + val) 0