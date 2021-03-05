import Data.Monoid
import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Foldable Tree where
   foldMap f (Leaf x)   = f x
   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

-- *Main> getSum (fold (Node (Leaf (Sum 3)) (Leaf (Sum 4))))
-- 7