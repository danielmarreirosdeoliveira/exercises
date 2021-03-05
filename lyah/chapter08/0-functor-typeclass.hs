import Prelude hiding (Functor, fmap)

class Functor f where
   fmap :: (a -> b) -> f a -> f b

instance Functor [] where
   fmap = map

-- *Main> fmap (*2) [1..3]
-- [2,4,6]

-- *Main> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

instance Functor Maybe where
   fmap f (Just x) = Just (f x)
   fmap f Nothing  = Nothing

-- *Main> fmap (*2) $ Just 3
-- Just 6
-- *Main> fmap (*2) $ Nothing
-- Nothing