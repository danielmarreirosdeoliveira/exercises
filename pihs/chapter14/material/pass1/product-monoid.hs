import Prelude hiding (Monoid, mempty, mappend, mconcat)


newtype Product a  = Product a
   deriving (Eq, Ord, Show, Read)

getProduct :: Product a -> a
getProduct (Product x) = x

-- *Main> getProduct (Product 3)
-- 3

class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty

instance Num a => Monoid (Product a) where
   -- mempty :: Sum a
   mempty = Product 1

   -- mappend :: Sum a -> Sum a -> Sum a
   Product x `mappend` Product y = Product (x*y)

-- *Main> getProduct (mconcat [Product 3, Product 4])
-- 12




