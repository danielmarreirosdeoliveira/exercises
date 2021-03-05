data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))



-- occurs :: Eq a => a -> Tree a -> Bool
-- occurs x (Leaf y)     = x == y
-- occurs x (Node l y r) = x == y || occurs x l || occurs x r

-- occurs0 :: Ord a => a -> Tree a -> Bool
-- occurs0 x (Leaf y)                 = x == y
-- occurs0 x (Node l y r) | x == y    = True
--                        | x < y     = occurs0 x l
--                        | otherwise = occurs0 x r





----

-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering

-- occursForSearchTrees

-- occurs :: Ord a => a -> Tree a -> Bool
-- occurs x (Leaf y)                          = x == y
-- occurs x (Node l y r)     | ordering == EQ = True
--                           | ordering == LT = occurs x l
--                           | ordering == GT = occurs x r
--                           where ordering = compare x y

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                          EQ -> True
                          LT -> occurs x l
                          GT -> occurs x r



-- the book states that it is more efficient because only one comparison is necessary,
-- but inside compare the same comparisons must take place if i'm right










