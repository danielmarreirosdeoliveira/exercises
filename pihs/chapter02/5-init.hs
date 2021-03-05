init' [_] = []
init' (x:xs) = [x] ++ init' xs

init'' xs = reverse (tail (reverse xs))