import Data.Monoid
import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show


instance Foldable Tree where
   -- fold :: Monoid a => Tree a -> a
   fold (Leaf x)   = x
   fold (Node l r) = fold l `mappend` fold r

   -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
   foldMap f (Leaf x)   = f x
   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

   -- foldr :: (a -> b -> a) -> b -> Tree a -> b
   foldr f v (Leaf x)   = f x v
   foldr f v (Node l r) = foldr f (foldr f v r) l

   -- foldl :: (a -> b -> a) -> a -> Tree b -> a
   foldl f v (Leaf x)   = f v x
   foldl f v (Node l r) = foldl f (foldl f v l) r

-- *Main> getSum (foldMap Sum (Node (Leaf 3) (Leaf 4)))
-- 7
