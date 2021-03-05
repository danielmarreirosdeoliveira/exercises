import Prelude hiding (Functor, Applicative, fmap, pure, (<*>))

class Functor f where
   fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

instance Functor ((->) a) where
   fmap = (.)

instance Applicative ((->) a) where
   -- pure :: a -> ((->) a)
   -- pure :: a -> (a ->)
   pure = const

   -- (<*>) :: (((->) a) (b -> c)) -> (((->) a) b) -> (((->) a) c)
   -- (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
   pf <*> px = \x -> pf x (px x)

-- (pure (+3) <*> (2*)) 17
-- 37

-- > (const 3) 5
-- 3
-- > ((const (*2)) 5) 3
-- 6