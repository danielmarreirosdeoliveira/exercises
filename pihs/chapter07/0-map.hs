

map0 :: (a -> b) -> [a] -> [b]
map0 f xs = [f x | x <- xs]

-- *Main> map0 (*2) [5,7]
-- [10,14]

-- *Main> map0 reverse ["abc","def"]
-- ["cba","fed"]

-- *Main> map0 (map0 (+1)) [[1,2,3],[4,5]]
-- [[2,3,4],[5,6]]

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = f x : map1 f xs