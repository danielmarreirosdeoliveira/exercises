replicate0 :: Int -> a -> [a]
replicate0 n x = [x | _ <- [1..n]]