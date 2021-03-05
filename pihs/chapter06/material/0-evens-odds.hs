


evens0 :: [a] -> [a]
evens0 [] = []
evens0 (x:xs) = x : odds0 xs

odds0 :: [a] -> [a]
odds0 [] = []
odds0 (_:xs) = evens0 xs