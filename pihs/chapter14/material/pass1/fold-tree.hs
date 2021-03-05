import Data.Monoid

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

fold :: Monoid a => Tree a -> a
fold (Leaf x)   = x
fold (Node l r) = fold l `mappend` fold r


t = Node (Node (Leaf (Sum 1)) (Leaf (Sum 1))) (Leaf (Sum 1))

-- *Main> getSum (fold t)
-- 3
