import Data.Monoid
import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Foldable Tree where
   fold (Leaf x)   = x
   fold (Node l r) = fold l `mappend` fold r
   foldMap f (Leaf x)   = f x
   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r
   foldr f v (Leaf x)   = f x v
   foldr f v (Node l r) = foldr f (foldr f v r) l
   foldl f v (Leaf x)   = f v x
   foldl f v (Node l r) = foldl f (foldl f v l) r


-- *Main> null (Node (Leaf 3) (Leaf 4))
-- False
-- *Main> null []
-- True
-- *Main> length [1..10]
-- 10
-- *Main> length (Node (Leaf 3) (Leaf 4))
-- 2
-- *Main> sum (Node (Leaf 3) (Leaf 4))
-- 7
-- *Main> maximum (Node (Leaf 3) (Leaf 4))
-- 4
-- *Main> foldr1 (+) [1..10]
-- 55
-- *Main> foldr1 (+) (Node (Leaf 3) (Leaf 4))
-- 7
-- *Main> toList (Node (Leaf 3) (Leaf 4))
-- [3,4]



