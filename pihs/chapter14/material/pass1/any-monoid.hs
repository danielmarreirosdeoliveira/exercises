import Prelude hiding (Monoid, mempty, mappend, mconcat)

newtype Any = Any { getAny :: Bool }
   deriving (Eq, Ord, Show, Read, Bounded)

class Monoid a where
   mempty  :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty

instance Monoid Any where
   -- mempty :: Sum a
   mempty = Any False

   -- mappend :: Any a -> Any a -> Any a
   Any x `mappend` Any y = Any (x || y)

-- *Main> getAny (mconcat [Any True, Any False, Any False])
-- True
-- *Main> getAny (mconcat [Any False, Any False, Any False])
-- False






