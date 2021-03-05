import Prelude hiding (Monoid, mempty, mappend, mconcat)

class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

instance Monoid Int where
   mempty = 0
   mappend = (+)

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

foldTree :: Monoid a => Tree a -> a
foldTree (Leaf a)     = a
foldTree (Node t1 t2) = (foldTree t1) `mappend` (foldTree t2)

zipTree :: Monoid a => Tree a -> Tree a -> Tree a
zipTree (Leaf a) (Leaf b)       = Leaf (a `mappend` b)
zipTree t (Leaf b) = Leaf ((foldTree t) `mappend` b)
zipTree (Leaf a) t =  Leaf (a `mappend` (foldTree t))
zipTree (Node ta1 ta2) (Node tb1 tb2) = Node (ta1 `zipTree` tb1) (ta1 `zipTree` tb2)


instance Monoid a => Monoid (Tree a) where
   mempty  = Leaf mempty
   mappend = zipTree

-- *Main> mempty :: Tree Int
-- Leaf 0

-- *Main> (Node (Leaf(3 :: Int)) (Leaf (3 :: Int))) `mappend` (Node (Leaf (5::Int)) (Leaf (4 :: Int)))
-- Node (Leaf 8) (Leaf 7)



