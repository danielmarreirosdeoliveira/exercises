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

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [e | (ls,rs) <- split ns,
                        lx <- results ls,
                        ry <- results rs,
                        e  <- combine lx ry]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices' ns, (e,m) <- results ns', m == n]



--

choices' :: [a] -> [[a]]
-- choices = concat . map perms . subs
choices' ns = [cs | sub <- subs ns,
                    cs  <- perms sub]