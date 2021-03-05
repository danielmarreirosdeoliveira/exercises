import Prelude hiding (Traversable, traverse)

-- usually t is also required to be functorial              TODO review why we can omit this here
class (Foldable t) => Traversable t where
   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Foldable Tree where
   foldMap f (Leaf x)   = f x
   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

instance Traversable Tree where
   -- traverse :: Applicative f =>
   --    (a -> f b) -> Tree a -> f (Tree b)
   traverse g (Leaf x)   = pure Leaf <*> g x
   traverse g (Node l r) =
      pure Node <*> traverse g l <*> traverse g r

-- *Main> traverse dec (Node (Leaf 1) (Leaf 2))
-- Just (Node (Leaf 0) (Leaf 1))
-- *Main> traverse dec (Node (Leaf 0) (Leaf 1))
-- Nothing

