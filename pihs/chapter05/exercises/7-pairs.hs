pairs0 = [(x, y) | x <- [1, 2], y <- [3, 4]]

pairs1 = concat [[(x, 3), (x, 4)] | x <- [1, 2] ]
