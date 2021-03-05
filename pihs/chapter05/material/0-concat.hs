concat0 :: [[a]] -> [a]
concat0 xss = [x | xs <- xss, x <- xs]