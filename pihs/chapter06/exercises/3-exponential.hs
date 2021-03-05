(^!) :: Num a => Integral b => a -> b -> a
x ^! 0 = 1
x ^! n = x * (x ^! (n-1))