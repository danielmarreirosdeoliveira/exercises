import Countdown





exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                       l <- exprs ls,
                       r <- exprs rs,
                       e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]




---- choices, exprs, eval
----
---- combinations over
l = [1,3,7,10,25,50] :: [Int]


possibleExprs n = length (concat (map exprs (choices n)))
-- ghci> possibleExprs l
-- 33655406

evaluatableExprs n = length (concat (map eval (concat (map exprs (choices n)))))
-- ghci> evaluatableExprs l (takes quite some time)
-- 4672540



-------- optimized

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [e | (ls,rs) <- split ns,
                        lx <- results ls,
                        ry <- results rs,
                        e  <- combine' lx ry] -- invalid expressions will now not be passed up for building more complex expressions (which then are invalid, too)

evaluatableExprs' n = length (concat (map results (choices n)))
-- ghci> evaluatableExprs' l (quicker now)
-- 4672540