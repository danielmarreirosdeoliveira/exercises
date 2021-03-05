data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
   -- fmap :: (a -> b) -> Expr a -> Expr b
   fmap f (Var x)   = Var (f x)
   fmap f (Val i)   = Val i
   fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
   -- pure :: a -> Expr a
   pure = Var

   -- (<*>) :: (Expr (a -> b)) -> (Expr a) -> (Expr b)
   (Var f) <*> (Var x)         = Var (f x)
   (Var f) <*> (Add xl xr)     = Add ((Var f) <*> xl) ((Var f) <*> xr)
   (Add fl fr) <*> (Add xl xr) = Add (fl <*> xl) (fr <*> xr)

-- > pure (*3) <*> (Var 7)
-- Var 21
-- > (Add (Var (*3)) (Var (*4))) <*> (Add (Var 10) (Var 20))
-- Add (Var 30) (Var 80)
-- > Var (*3) <*> (Add (Var 10) (Var 20))
-- Add (Var 30) (Var 60)
-- >  Var (*) <*> (Add (Var 10) (Var 20)) <*> (Add (Var 20) (Var 20))
-- Add (Var 200) (Var 400)
-- > pure (*) <*> (Add (Var 10) (Var 20)) <*> (Add (Var 20) (Var 20))
-- Add (Var 200) (Var 400)

