import Prelude hiding (Monoid, mempty, mappend, mconcat)


newtype Sum a  = Sum a
   deriving (Eq, Ord, Show, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

-- *Main> getSum (Sum 3)
-- 3

class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty

instance Num a => Monoid (Sum a) where
   -- mempty :: Sum a
   mempty = Sum 0

   -- mappend :: Sum a -> Sum a -> Sum a
   Sum x `mappend` Sum y = Sum (x+y)

-- *Main> mconcat [Sum 3, Sum 4, Sum 5]
-- Sum 12

