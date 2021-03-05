import Prelude hiding (Functor, Applicative, Maybe, Just, Nothing, fmap, pure, (<*>), sequenceA)

data Maybe a = Just a | Nothing deriving Show

class Functor f where
   fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

--
instance Functor IO where
   fmap f m = do x <- m
                 return (f x)

instance Applicative IO where
   -- pure :: a -> IO a
   pure = return

   -- (<*>) :: IO (a -> b) -> IO a -> IO b
   mg <*> mx = do {g <- mg; x <- mx; return (g x)}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

-- *Main> getChars 4
-- asdf"asdf"


-- effectful programming
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

getChars' :: Int -> IO String
getChars' n = sequence (replicate n getChar)

-- *Main> getChars 4
-- abcd"abcd"

