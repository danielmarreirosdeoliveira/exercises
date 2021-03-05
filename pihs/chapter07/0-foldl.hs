


sum0 :: Num a => [a] -> a
sum0 = sum' 0
       where
         sum' v [] = v
         sum' v (x:xs) = sum' (v+x) xs



foldl0 :: (a -> b -> a) -> a -> [b] -> a
foldl0 f v [] = v
foldl0 f v (x:xs) = foldl0 f (f v x) xs

sum1 = foldl0 (+) 0