import Prelude hiding (Monoid, mempty, mappend, mconcat)

newtype All = All { getAll :: Bool }
   deriving (Eq, Ord, Show, Read, Bounded)

class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty

instance Monoid All where
   -- mempty :: Sum a
   mempty = All True

   -- mappend :: All a -> All a -> All a
   All x `mappend` All y = All (x && y)

-- *Main> getAll (mconcat [All True, All True, All True])
-- True
-- *Main> getAll (mconcat [All True, All True, All False])
-- False





