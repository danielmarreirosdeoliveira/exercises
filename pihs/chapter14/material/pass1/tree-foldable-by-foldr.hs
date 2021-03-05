import Data.Monoid
import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Foldable Tree where
   foldr f v (Leaf x)   = f x v
   foldr f v (Node l r) = foldr f (foldr f v r) l

-- *Main> toList (Node (Leaf 3) (Leaf 4))
-- [3,4]
-- *Main> getSum (foldMap Sum (Node (Leaf 3) (Leaf 4)))
-- 7
