data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
   -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap f Leaf         = Leaf
   fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

-- > t = Node Leaf 13 Leaf
-- > t
-- Node Leaf 13 Leaf
-- > fmap (*2) t
-- Node Leaf 26 Leaf

