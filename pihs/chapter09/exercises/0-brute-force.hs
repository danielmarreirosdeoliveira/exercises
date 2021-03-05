import Countdown

main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)
-- *Main> solutions [1,3,2] 4
-- [1+3,3+1,(3-1)+2,(3-1)*2,3+(2-1),(3+2)-1,(2-1)+3,2+(3-1),2*(3-1),(2+3)-1]

---- Brute force solution

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                       l <- exprs ls,
                       r <- exprs rs,
                       e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]





