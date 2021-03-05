data Expr = Val Int | Div Expr Expr

eval' :: Expr -> Int
eval' (Val n)   = n
eval' (Div x y) = eval' x `div` eval' y

-- *Main> eval (Val 3)
-- 3
-- *Main> eval (Div (Val 3) (Val 2))
-- 1
-- *Main> eval (Div (Val 3) (Val 0))
-- *** Exception: divide by zero

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just n  -> case eval y of
                                  Nothing -> Nothing
                                  Just m  -> safediv n m

-- *Main> eval (Div (Val 1) (Val 2))
-- Just 0
-- *Main> eval (Div (Val 1) (Val 0))
-- Nothing





