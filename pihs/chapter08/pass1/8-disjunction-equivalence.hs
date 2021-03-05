type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)
----

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
          | Imply Prop Prop
          deriving Show



type Subst = Assoc Char Bool


eval :: Subst -> Prop -> Bool
eval _ (Const b)           = b
eval s (Var x)             = find x s
eval s (Not p)             = not (eval s p)
eval s (And p q)           = eval s p && eval s q
eval s (Imply p q)         = eval s p <= eval s q
eval s (Equiv p q)         = eval s p == eval s q
eval s (Or p q)            = eval s p || eval s q

subst :: Subst
subst = [('A', False), ('B', True)]


vars :: Prop -> [Char]
vars (Const _)    = []
vars (Var x)      = [x]
vars (Not p)      = vars p
vars (And p q)    = vars p ++ vars q
vars (Imply p q)  = vars p ++ vars q
vars (Equiv p q)  = vars p ++ vars q
vars (Or p q)     = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

----

p5 = Equiv (Var 'A') (Var 'A')
p6 = Equiv (Var 'A') (Var 'B')

p7 = Or (Var 'A') (Var 'B')
p8 = Or (Var 'A') (Var 'A')
p9 = Or (Var 'B') (Var 'B')

p10 = Imply (Or (Var 'B') (Var 'B')) (Var 'B')




