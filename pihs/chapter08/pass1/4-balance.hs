data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show



l1 = [1,7]
l2 = [1,7,19]
l3 = [1,7,19,11,23]
l4 = [1,7,19,11,23,17]



split :: [a] -> [[a]]
split xs = [take h xs, drop h xs]
         where h = (length xs) `div` 2


balance :: [a] -> Tree a
balance []                   = error "Invalid param"
balance [x]                  = Leaf x
balance xs | null l          = balance r
           | null r          = balance l
           | otherwise       = Node (balance l) (balance r)
           where
                 [l, r] = split xs

