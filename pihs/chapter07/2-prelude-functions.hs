all0 :: (a -> Bool) -> [a] -> Bool
all0 f = foldr (\val acc -> f(val) && acc) True

any0 :: (a -> Bool) -> [a] -> Bool
any0 f = foldr (\val acc -> f(val) || acc) False

takeWhile0 :: (a -> Bool) -> [a] -> [a]
takeWhile0  _ []                = []
takeWhile0 p (x:xs)
                    | p x       = x : takeWhile0 p xs
                    | otherwise = []

dropWhile0 :: (a -> Bool) -> [a] -> [a]
dropWhile0 _ []                 = []
dropWhile0 p xs@(x:xs')
                    | p x       = dropWhile0 p xs'
                    | otherwise = xs

all1 p = and . map p
any1 p = or . map p