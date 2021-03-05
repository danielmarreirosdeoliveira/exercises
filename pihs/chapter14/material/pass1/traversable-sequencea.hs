import Prelude hiding (Traversable, traverse, sequenceA)

-- usually t is also required to be functorial              TODO review why we can omit this here
class (Foldable t) => Traversable t where
   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
   sequenceA :: Applicative f => t (f a) -> f (t a)
   sequenceA = traverse id

instance Traversable [] where
   -- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
   traverse g []     = pure []
   traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

-- *Main> sequenceA [Just 1, Just 2, Just 3]
-- Just [1,2,3]
-- *Main> sequenceA [Just 1, Just 2, Nothing]
-- Nothing

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

-- *Main> sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2)))
-- Just (Node (Leaf 1) (Leaf 2))
-- *Main> sequenceA (Node (Leaf (Just 1)) (Leaf Nothing))
-- Nothing


