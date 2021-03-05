import Prelude hiding (Monoid, mempty, mappend, mconcat)


class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty


instance Monoid [a] where
   -- mempty :: [a]
   mempty = []

   -- mappend :: [a] -> [a] -> [a]
   mappend = (++)


-- *Main> mconcat [[1, 2], [2, 5], [5, 6]]
-- [1,2,2,5,5,6]
