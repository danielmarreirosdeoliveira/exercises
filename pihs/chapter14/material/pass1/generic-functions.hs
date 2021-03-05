import Prelude hiding (and, or, any, all, concat)
import Data.Monoid
import Data.Foldable hiding (and, or, any, all, concat)

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Foldable Tree where
   foldMap f (Leaf x)   = f x
   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

-- average :: [Int] -> Int
average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

-- *Main> average [1..3]
-- 2
-- *Main> average (Node (Leaf 4) (Leaf 6))
-- 5

and :: Foldable t => t Bool -> Bool
and = getAll . foldMap All

or :: Foldable t => t Bool -> Bool
or = getAny . foldMap Any

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)

any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)

-- *Main> and [True,False,True]
-- False

-- *Main> or (Node (Leaf True) (Leaf True))
-- True

-- *Main> all even [1,2,3]
-- False

-- *Main> any even (Node (Leaf 1) (Leaf 2))
-- True

concat :: Foldable t => t [a] -> [a]
concat = fold

-- *Main> concat ["ab","cd","ef"]
-- "abcdef"

-- *Main> concat (Node (Leaf [1,2]) (Leaf [3]))
-- [1,2,3]