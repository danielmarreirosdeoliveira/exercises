


reverse0 :: [a] -> [a]
reverse0 [] = []
reverse0 (x:xs) = reverse0 xs ++ [x]