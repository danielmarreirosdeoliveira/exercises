import Prelude hiding (Monad, (>>=), return, mapM)
import Data.Char

class Applicative m => Monad m where
   return :: a -> m a
   (>>=)  :: m a -> (a -> m b) -> m b
   return = pure

instance Monad Maybe where
   -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ = Nothing
   (Just x) >>= f = f x

instance Monad [] where
   -- (>>=) :: [a] -> (a -> [b]) -> [b]
   xs >>= f = [y | x <- xs, y <- f x]

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- commenting in declaration does not work, type is
-- > :t mapM
-- mapM :: (Monad m, GHC.Base.Monad m) => (t -> m a) -> [t] -> m [a]
mapM f []     = return []
mapM f (x:xs) = do y  <- f x
                   ys <- mapM f xs
                   return (y:ys)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- > mapM conv "1234"
-- Just [1,2,3,4]
-- > mapM conv "123a"
-- Nothing

-- join :: Monad m => (m (m a)) -> m a
-- see above, same here
join mmx = do mx <- mmx
              x  <- mx
              return x

-- > join $ Just (Just 3)
-- Just 3
-- > join $ Just Nothing
-- Nothing
-- > join $ [[1,2],[3,4]]
-- [1,2,3,4]

-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- commenting in declaration does not work
filterM p []     = return []
filterM p (x:xs) = do b  <- p x
                      ys <- filterM p xs
                      return (if b then x:ys else ys)

-- > filterM (\x -> [True, False]) [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

filterM' p []     = return []
filterM' p (x:xs) = p x           >>= \b  ->
                    filterM' p xs >>= \ys ->
                    return (if b then x:ys else ys)

-- > filterM' (\x -> [True, False]) [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
