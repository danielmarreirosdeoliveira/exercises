firsts :: [(a,b)] -> [a]
firsts xs = [x | (x, _) <- xs]