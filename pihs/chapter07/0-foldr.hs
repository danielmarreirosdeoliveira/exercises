


foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 _ v []     = v
foldr0 f v (x:xs) = f x (foldr0 f v xs)

sum0 :: Num a => [a] -> a
sum0 = foldr0 (+) 0

product0 :: Num a => [a] -> a
product0 = foldr0 (*) 1

and0 :: [Bool] -> Bool
and0 = foldr0 (&&) True

or0 :: [Bool] -> Bool
or0 = foldr0 (||) False

length0 :: [a] -> Int
length0 = foldr0 (\_ n -> 1 + n) 0

snoc0 x xs = xs ++ [x]

reverse0 :: [a] -> [a]
reverse0 = foldr0 snoc0 []

-- foldr (#) v [x0, x1, ..., xn] = x0 # ( x1 # ( ... ( xn # v ) )