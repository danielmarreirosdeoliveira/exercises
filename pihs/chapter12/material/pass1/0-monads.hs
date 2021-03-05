data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

-- ghci> eval (Div (Val 1) (Val 0))
-- *** Exception: divide by zero

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                     Nothing -> Nothing
                     Just n  -> case eval' y of
                                   Nothing -> Nothing
                                   Just m  -> safediv n m

--ghci> eval' (Div (Val 4) (Val 2))
--Just 2
--ghci> eval' (Div (Val 4) (Val 0))
--Nothing

(>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>>= f = case mx of
               Nothing -> Nothing
               Just x  -> f x

eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = eval'' x >>>= \n ->
                   eval'' y >>>= \m ->
                   safediv n m

--ghci> eval'' (Div (Val 4) (Val 2))
--Just 2
--ghci> eval'' (Div (Val 4) (Val 0))
--Nothing
--ghci> eval'' (Div (Div (Val 3) (Val 0)) (Val 3))
--Nothing



eval''' :: Expr -> Maybe Int
eval''' (Val n)   = Just n
eval''' (Div x y) = do n <- eval''' x
                       m <- eval''' y
                       safediv n m


--ghci> eval''' (Div (Val 4) (Val 0))
--Nothing
--ghci> eval''' (Div (Val 4) (Val 2))
--Just 2
--ghci> eval''' (Div (Div (Val 3) (Val 0)) (Val 3))
--Nothing







