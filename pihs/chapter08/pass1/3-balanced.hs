data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 4))
         (Node (Leaf 6) (Leaf 9))

t2 :: Tree Int
t2 = Node (Node (Leaf 1) (Leaf 4))
         (Leaf 6)

t3 :: Tree Int
t3 = Node (Node (Leaf 1) (Node (Leaf 8) (Leaf 9)))
         (Leaf 6)


count :: Tree a -> Int
count (Leaf _)   = 1
count (Node l r) = count l + count r


balanced :: Tree a -> Bool
balanced (Leaf _)                            = True
balanced (Node l r) | count l - count r <= 1 = True
                    | otherwise              = False












