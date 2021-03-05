factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n],
                  sum [y | y <- factors x, y /= x] == x] -- could it be written somewhat cleaner, pulling out the inner comprehension?