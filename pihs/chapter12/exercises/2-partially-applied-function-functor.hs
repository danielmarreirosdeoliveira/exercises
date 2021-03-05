import Prelude hiding (Functor, fmap)

class Functor f where
   fmap :: (a -> b) -> f a -> f b

instance Functor ((->) a) where
   -- fmap (b -> c) -> ((->) a b) -> ((->) b c)
   -- fmap (b -> c) -> (a -> b) -> (b -> c)
   -- fmap f p = f . p
   fmap = (.)

-- > (fmap (+3) (*2)) 17
-- 37
