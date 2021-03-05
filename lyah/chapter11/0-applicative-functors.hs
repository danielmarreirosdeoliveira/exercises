import Prelude hiding (Functor, Applicative, fmap, pure, (<*>), (<$>))

-- applicative functors are used if we want to map over functions which take multiple parameters

-- *Main> let a = fmap (*) [1,2,3,4]
-- *Main> :t a
-- a :: Num a => [a -> a]
-- *Main> fmap (\f -> f 2) a
-- [2,4,6,8]

class Functor f where
   fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
   pure   :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

instance Functor [] where
   fmap = map

instance Applicative [] where
   -- pure  :: a -> [a]
   pure a = [a]
   -- (<*>) :: [a -> b] -> [a] -> [b]
   fs <*> xs = [f x | f <- fs, x <- xs]

-- pure (*) <*> [3] <*> [4]
-- [12]
-- pure (*) <*> [3, 4] <*> [4, 5]
-- [12,15,16,20]

-- my alternative version
   -- pure = repeat
   -- fs <*> xs = map (\(f, x) -> f x) (zip fs xs)

-- now elements with the same indices are mapped
-- *Main> pure (*) <*> [3, 4] <*> [4, 5]
-- [12,20]
-- *Main> pure (*) <*> [3, 4] <*> [4]
-- [12]
-- *Main> pure (*) <*> [3..7] <*> [4..5]
-- [12,20]

instance Functor Maybe where
   fmap f (Just x) = Just (f x)
   fmap f Nothing  = Nothing

instance Applicative Maybe where
   pure  = Just
   Nothing  <*> _         = Nothing
   (Just f) <*> something = fmap f something

-- *Main> pure (*) <*> Just 3 <*> Just 4
-- Just 12
-- *Main> sumabc a b c = a + b + c
-- pure sumabc <*> Just 3 <*> Just 4 <*> Just 5
-- Just 12

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- *Main> (*) <$> Just 3 <*> Just 4
-- Just 12
-- *Main> (++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"


-- *Main> pure "Hey" :: [String]
-- ["Hey"]
-- *Main> pure "Hey" :: Maybe String
-- Just "Hey"

-- [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]
-- [(+),(*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]

-- *Main> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- [ x*y | x <- [2,5,10], y <- [8,10,11]]
-- [16,20,22,40,50,55,80,100,110]
-- (*) <$> [2,5,10] <*> [8,10,11]
-- [16,20,22,40,50,55,80,100,110]

-- filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
-- [55,80,100,110]


newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)
-- *Main> ZipList "abc"
-- ZipList {getZipList = "abc"}

instance Functor ZipList where
   -- fmap :: (a -> b) -> f a -> f b
   fmap f (ZipList a) = ZipList (map f a)

-- *Main> fmap (*3) (ZipList [1,2,3])
-- ZipList {getZipList = [3,6,9]}
instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- *Main> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [2,3,4]
-- [3,5,7]

