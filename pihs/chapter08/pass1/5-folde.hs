data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val v)         = f v
folde f g (Add l r)       = g (folde f g l) (folde f g r)




-- *Main> folde (\x -> 'a') (\x -> id) (Val 3)
-- 'a'

-- *Main> folde (*2) (\x -> \y -> x + y) (Add (Val 3) (Add (Val 7) (Val 4)))
-- 28

