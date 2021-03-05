import Prelude hiding (Monoid, mempty, mappend, mconcat)


class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty


instance Monoid Int where
   -- mempty :: Int
   mempty = 0

   -- mappend :: Int -> Int -> Int
   mappend = (+)
   -- alternatively
   -- mappend = (*)


-- *Main> (1 :: Int)  `mappend` (2 :: Int)
-- 3


