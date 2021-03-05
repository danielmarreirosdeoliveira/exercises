import Prelude hiding ((>>=))

data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = case eval'' x of
                      Nothing -> Nothing
                      Just n  -> case eval'' y of
                                    Nothing -> Nothing
                                    Just m  -> safediv n m

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
              Nothing -> Nothing
              Just x  -> f x

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = eval' x >>= \n ->
                  eval' y >>= \m ->
                  safediv n m

-- *Main> eval' (Div (Val 1) (Val 2))
-- Just 0
-- *Main> eval' (Div (Val 1) (Val 0))
-- Nothing

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m

-- *Main> eval (Div (Val 1) (Val 2))
-- Just 0
-- *Main> eval (Div (Val 1) (Val 0))
-- Nothing




