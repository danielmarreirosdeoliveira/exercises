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


instance Monoid a => Monoid (Maybe a) where
   -- mempty :: Maybe a
   mempty = Nothing

   -- mappend :: Maybe a -> Maybe a -> Maybe a
   Nothing `mappend` my      = my
   mx      `mappend` Nothing = mx
   Just x  `mappend` Just y  = Just (x `mappend` y)


-- *Main> Just (3 :: Int) `mappend` Just (4 :: Int)
-- Just 7




