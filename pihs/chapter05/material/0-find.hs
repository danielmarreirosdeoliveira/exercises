find0 :: Eq a => a -> [(a, b)] -> [b]
find0 k t = [v | (k', v) <- t, k' == k]
