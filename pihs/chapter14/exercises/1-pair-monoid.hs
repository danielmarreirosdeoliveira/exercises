import Prelude hiding (Monoid, mempty, mappend)

class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

instance Monoid Int where
   -- mempty :: Int
   mempty = 0
   -- mappend :: Int -> Int -> Int
   mappend = (+)

instance (Monoid a, Monoid b) => Monoid (a, b) where
   -- mempty :: (a,b)
   mempty = (mempty, mempty)
   -- mappend :: (a,b) -> (a,b) -> (a,b)
   (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- *Main> mempty :: (Int, Int)
-- (0,0)
-- *Main> ((3,2) :: (Int,Int)) `mappend` ((3,5) :: (Int,Int))
-- (6,7)
