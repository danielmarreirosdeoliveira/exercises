import Prelude hiding (Traversable, traverse)

class (Functor t, Foldable t) => Traversable t where
   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

instance Traversable [] where
   -- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
   traverse g []     = pure []
   traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs

-- *Main> traverse dec [0,1,2]
-- Nothing
-- *Main> traverse dec [1,2,3]
-- Just [0,1,2]