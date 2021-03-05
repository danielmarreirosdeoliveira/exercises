import Countdown

main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)

---- Exploiting algebraic properties

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

type Result = (Expr, Int)

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]




---- split also return pairs containing the empty list results in non-termination when calling solutions

split' :: [a] -> [([a], [a])]
split' []     = []
-- split' [_]    = [] <- for generalization we take this out
split' (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split' xs]
-- ghci> split' [1]
-- [([1],[])]






split'' :: [Int] -> [([Int], [Int])] -- to demonstrate it even more directly
split'' _ = [([1,2],[])]

-- i think the problem is that the left side gets passed into result,
-- which then gets split into itself and the empty list again, then passed
-- into results (lx <- results ls) again, and so on and so on ...

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [e | (ls,rs) <- split'' ns, -- split' ns,
                        lx <- results ls,
                        ry <- results rs,
                        e  <- combine lx ry]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]