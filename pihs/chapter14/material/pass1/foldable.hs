import Prelude hiding (Foldable, Monoid, mempty, mappend, mconcat, foldMap, foldl, foldr)

class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty

newtype Sum a  = Sum a
   deriving (Eq, Ord, Show, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid (Sum a) where
   -- mempty :: Sum a
   mempty = Sum 0

   -- mappend :: Sum a -> Sum a -> Sum a
   Sum x `mappend` Sum y = Sum (x+y)

instance Monoid [a] where
   -- mempty :: [a]
   mempty = []

   -- mappend :: [a] -> [a] -> [a]
   mappend = (++)

class Foldable t where
   fold      :: Monoid a => t a -> a
   foldMap   :: Monoid b => (a -> b) -> t a -> b
   foldr     :: (a -> b -> b) -> b -> t a -> b
   foldl     :: (a -> b -> a) -> a -> t b -> a

instance Foldable [] where
   -- fold          :: Monoid a => [a] -> a
   fold []          = mempty
   fold (x:xs)      = x `mappend` fold xs

   -- foldMap       :: Monoid b => (a -> b) -> [a] -> b
   foldMap _ []     = mempty
   foldMap f (x:xs) = f x `mappend` foldMap f xs

   -- foldr :: (a -> b -> b) -> b -> [a] -> b
   foldr _ v []     = v
   foldr f v (x:xs) = f x (foldr f v xs)

   -- foldl :: (a -> b -> a) -> a -> [b] -> a
   foldl _ v []     = v
   foldl f v (x:xs) = foldl f (f v x) xs

-- *Main> getSum (fold [Sum 1, Sum 2, Sum 3])
-- 6

-- *Main> getSum (foldMap Sum [1..3])
-- 6
-- *Main> getSum (fold (map Sum [1, 2, 3]))
-- 6

-- would work with Product accordingly
-- *Main> getProduct (foldMap Product [1..3])
-- 6


-- foldl and foldr do not require the underlying Monoid, which can be verified by
-- commenting out the List Monoid above and then running:
-- *Main> foldl (+) 0 [1, 2, 4, 5]
-- 12
-- *Main> foldr (+) 0 [1, 2, 4, 5]
-- 12
