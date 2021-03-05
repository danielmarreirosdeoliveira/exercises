import Countdown

main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)

---- Combining generation and evaluation

type Result = (Expr, Int)

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [e | (ls,rs) <- split ns,
                        lx <- results ls,
                        ry <- results rs,
                        e  <- combine lx ry] -- invalid expressions will now not be passed up for building more complex expressions (which then are invalid, too)


