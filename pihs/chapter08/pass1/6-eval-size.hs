data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val v)         = f v
folde f g (Add l r)       = g (folde f g l) (folde f g r)




eval :: Expr -> Int
eval = folde id (\x -> \y -> x + y)


-- *Main> eval (Add (Val 3) (Add (Val 7) (Val 4)))
-- 14



size :: Expr -> Int
size (Val _)   = 1
size (Add l r) = size l + size r




-- *Main> size (Add (Val 3) (Add (Val 7) (Val 4)))
-- 3


