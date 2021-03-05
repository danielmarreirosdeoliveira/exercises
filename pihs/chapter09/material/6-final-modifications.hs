import Data.List
data Op = Add | Sub | Mul | Div | Exp ---- <- we add Exponentiation to the mix

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = y == 0 || y > 1

type Result = (Expr, Int)

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [e | (ls,rs) <- split ns,
                        lx <- results ls,
                        ry <- results rs,
                        e  <- combine lx ry]

solutionResults :: [Int] -> Int -> [Result]
solutionResults ns n | not (null solutionsForN)                                = solutionsForN
                     | distanceToNextLHSSolutions < distanceToNextRHSSolutions = nextLHSSolutions
                     | otherwise                                               = nextRHSSolutions
                     where
                        solutionsForN = calcSolutions n
                        distanceToNextLHSSolutions = distanceToNextSolutions nextLHSSolutions
                        distanceToNextRHSSolutions = distanceToNextSolutions nextRHSSolutions
                        nextLHSSolutions = findNearbySolutions n (-)
                        nextRHSSolutions = findNearbySolutions n (+)
                        findNearbySolutions n op | null nearbySolutions = findNearbySolutions (op n 1) op
                                                 | otherwise            = nearbySolutions
                                                 where nearbySolutions  = calcSolutions (op n 1)
                        distanceToNextSolutions nextSolutions = abs (snd (head nextSolutions) - n)
                        calcSolutions n       = sort [(e,m) | ns' <- choices ns, (e,m) <- results ns', m == n]


solutions :: [Int] -> Int -> (Int, [Expr])
solutions ns n = (snd (head results), map fst results)
               where results = (solutionResults ns n)

-- old version
-- solutions' :: [Int] -> Int -> [Expr]
-- solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]


-- for example solutions' [1,2,4,9]
-- has a solution for 108 ([(1+2)*(4*9),4*((1+2)*9),9*((1+2)*4)])
-- and for 125,           ([(9-4)^(1+2)])
-- but no solutions in between
-- so:
--
-- ghci> solutions [1,2,4,9] 108
-- (108,[(1+2)*(4*9),4*((1+2)*9),9*((1+2)*4)])
--
-- ghci> solutions [1,2,4,9] 110
-- (108,[(1+2)*(4*9),4*((1+2)*9),9*((1+2)*4)])
--
-- ghci> solutions [1,2,4,9] 125
-- (125,[(9-4)^(1+2)])
--
-- ghci> solutions [1,2,4,9] 120
-- (125,[(9-4)^(1+2)])





---- order

-- optional
simpleOps :: Expr -> Int -- nr of Add or Mul
simpleOps (Val _)       = 0
simpleOps (App Add x y) = 1 + simpleOps x + simpleOps y
simpleOps (App Mul x y) = 1 + simpleOps x + simpleOps y
simpleOps (App _ x y)   = simpleOps x + simpleOps y


instance Eq Expr where -- just a dummy
  _ == _ = True

instance Ord Expr where -- we sort by number of values
  compare l r | cl < cr   = LT
              | cl > cr   = GT
              | sol > sor = LT -- optional
              | sol < sor = GT -- optional
              | otherwise = EQ
              where
                cl  = length (values l)
                cr  = length (values r)
                sor = simpleOps r
                sol = simpleOps l

-- ghci> sort [(Val 1), (App Add (Val 2) (Val 3))]
-- [1,2+3]

-- ghci> sort [(App Div (Val 2) (Val 3)), (App Mul (Val 2) (Val 3)), (App Sub (Val 2) (Val 3)), (App Add (Val 2) (Val 3))]
-- [2*3,2+3,2/3,2-3]

-- ghci> solutions [2,3,4,13,7] 27
-- ...

