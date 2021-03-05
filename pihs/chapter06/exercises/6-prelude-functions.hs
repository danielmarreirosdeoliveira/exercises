and0 :: [Bool] -> Bool
and0 [] = True
and0 (x:xs) = x && and0 xs

concat0 :: [[a]] -> [a]
concat0 [] = []
concat0 (x:xs) = x ++ concat0(xs)

replicate0 :: Int -> a -> [a]
replicate0 0 _ = []
replicate0 n x = x : replicate0 (n-1) x

(!!-) :: [a] -> Int -> a -- nth element of list
(x:xs) !!- 0 = x
(_:xs) !!- n = xs !!- (n-1)

elem0 :: Eq a => a -> [a] -> Bool
elem0 _ [] = False
elem0 x' (x:xs) = (x' == x) || elem0 x' xs



