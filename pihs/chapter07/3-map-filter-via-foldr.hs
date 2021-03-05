
map0 :: (a -> b) -> [a] -> [b]
map0 f = foldr (\x xs -> f x : xs) []


filter0 :: (a -> Bool) -> [a] -> [a]
filter0 p = foldr (\x xs -> if p x then x:xs else xs) []