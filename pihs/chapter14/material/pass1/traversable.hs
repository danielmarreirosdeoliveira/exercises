import Prelude hiding (traverse)

-- the idea of mapping a function over a list can be generalized further

-- map :: (a -> b) -> [a] -> [b]
traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
-- traverse g []     = pure []
-- traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs
traverse g []     = Just []
traverse g (x:xs) = (:) <$> g x <*> traverse g xs

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

-- *Main> traverse dec [1,2,3]
-- Just [0,1,2]
-- *Main> traverse dec [2,1,0]
-- Nothing
