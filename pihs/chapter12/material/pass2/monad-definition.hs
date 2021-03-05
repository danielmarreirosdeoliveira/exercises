import Prelude hiding (Monad, (>>=), return)

class Applicative m => Monad m where
   return :: a -> m a
   (>>=)  :: m a -> (a -> m b) -> m b
   return = pure

instance Monad Maybe where
   -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ = Nothing
   (Just x) >>= f = f x

data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = eval' x >>= \n ->
                  eval' y >>= \m ->
                  safediv n m

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m

-- *Main> eval' (Div (Val 4) (Val 2))
-- Just 2
-- *Main> eval (Div (Val 4) (Val 2))
-- Just 2
-- *Main> eval (Div (Div (Val 4) (Val 0)) (Val 2))
-- Nothing
-- *Main> eval (Div (Div (Val 4) (Val 2)) (Val 0))
-- Nothing
-- *Main> eval (Div (Div (Val 4) (Val 2)) (Val 2))
-- Just 1

instance Monad [] where
   -- (>>=) :: [a] -> (a -> [b]) -> [b]
   xs >>= f = [y | x <- xs, y <- f x]

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

-- *Main> pairs [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]



