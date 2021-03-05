rmFirst :: Eq a => a -> [a] -> [a]
rmFirst _ []                  = []
rmFirst x (x':xs) | x == x'   = xs
                  | otherwise = x' : rmFirst x xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _ []      = False
isChoice (x:xs) ys = elem x ys && isChoice xs (rmFirst x ys)
