positions0 :: Eq a => a -> [a] -> [Int]
positions0 x xs = [i | (x', i) <- zip xs [0..], x' == x]

find0 :: Eq a => a -> [(a, b)] -> [b]
find0 k t = [v | (k', v) <- t, k' == k]

positions1 :: Eq a => a -> [a] -> [Int]
positions1 x xs = find0 x [z | z <- zip xs [0..]]
