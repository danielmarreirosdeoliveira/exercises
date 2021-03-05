import Prelude hiding (Functor, Applicative, Maybe, Just, Nothing, fmap, pure, (<*>))

data Maybe a = Just a | Nothing deriving Show

class Functor f where
   fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

instance Functor Maybe where
   -- fmap :: (a -> b) -> Maybe a -> Maybe b
   fmap _ Nothing  = Nothing
   fmap g (Just a) = Just (g a)

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing  <*> _  = Nothing
  (Just g) <*> mx = fmap g mx

-- *Main> pure (+1) <*> Just 1
-- Just 2

-- *Main> pure (+) <*> Just 1 <*> Just 2
-- Just 3

-- *Main> pure (+) <*> Nothing <*> Just 2
-- Nothing