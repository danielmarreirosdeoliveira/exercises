luhnDouble :: Int -> Int
luhnDouble x | x2 > 9 = x2 - 9
             | otherwise = x2
             where x2 = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z a = total `mod` 10 == 0
      where total = (luhnDouble x) + y + (luhnDouble z) + a