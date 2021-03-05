bools :: [Bool]
bools = [True, True, False]

nums :: [[Int]]
nums = [[1, 4], [7, 7, 1]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a