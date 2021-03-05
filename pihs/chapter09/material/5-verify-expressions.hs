import Countdown




type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [e | (ls,rs) <- split ns,
                        lx <- results ls,
                        ry <- results rs,
                        e  <- combine' lx ry] -- invalid expressions will now not be passed up for building more complex expressions (which then are invalid, too)




valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub _ _ = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && x `mod` y == 0




l = [1,3,7,10,25,50] :: [Int]

evaluatableExprs n = length (concat (map results (choices n)))
-- ghci> evaluatableExprs l
-- 10839369