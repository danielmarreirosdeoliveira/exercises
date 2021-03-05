




even0 :: Int -> Bool
even0 0 = True
even0 n = odd0 (n-1)

odd0 :: Int -> Bool
odd0 0 = False
odd0 n = even (n-1)