


fib0 :: Int -> Int
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n-2) + fib0 (n-1)









reverse0 :: [a] -> [a]
reverse0 [] = []
reverse0 (x:xs) = reverse0 xs ++ [x]

fib2 :: Int -> [Int] -> [Int]
fib2 0 (l1:l2:r) = ((l1+l2):(l1:l2:r))
fib2 n (l1:l2:r) = fib2 (n-1) ((l1+l2):(l1:l2:r))

fib1 :: Int -> [Int]
fib1 0 = [0]
fib1 1 = [0, 1]
fib1 n = reverse0(fib2 (n-3) [1,0])

