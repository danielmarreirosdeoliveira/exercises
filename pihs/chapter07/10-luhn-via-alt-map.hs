altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []     = []
altMap f g (x:[]) = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

luhnDouble :: Int -> Int
luhnDouble x | x2 > 9    = x2 - 9
             | otherwise = x2
             where x2    = x * 2

luhn0 :: Int -> Int -> Int -> Int -> Bool
luhn0 x y z a = total `mod` 10 == 0
                where total = (luhnDouble x) + y + (luhnDouble z) + a

luhn1 :: [Int] -> Bool
luhn1 digits = total `mod` 10 == 0
                where total = sum (altMap luhnDouble id digits)