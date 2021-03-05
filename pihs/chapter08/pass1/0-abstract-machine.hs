data Expr = Val Int | Add Expr Expr deriving Show

----

value' :: Expr -> Int
value' (Val n)   = n
value' (Add x y) = value' x + value' y

-- *Main> (2 + 3) + 4
-- 9
-- *Main> value (Add (Add (Val 2) (Val 3)) (Val 4))
-- 9


----

type Cont = [Op]

data Op = EVAL Expr | ADD Int deriving Show


eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

value :: Expr -> Int
value e = eval e []

-- *Main> value (Add (Val 4) (Add (Val 1) (Val 5)))
-- 10
