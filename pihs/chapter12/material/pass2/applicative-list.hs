import Prelude hiding (Functor, Applicative, Maybe, Just, Nothing, fmap, pure, (<*>))

data Maybe a = Just a | Nothing deriving Show

class Functor f where
   fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

--
instance Functor [] where
   -- fmap :: (a -> b) -> [a] -> [b]
   fmap = map

instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]

  -- (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [g x | g <- gs, x <- xs]

-- *Main> pure (+1) <*> [1,2,3]
-- [2,3,4]

-- *Main> pure (+) <*> [1] <*> [2]
-- [3]

-- *Main> pure (*) <*> [1,2] <*> [3,4]
-- [3,4,6,8]

--
prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = [x*y | x <- xs, y <- ys]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

