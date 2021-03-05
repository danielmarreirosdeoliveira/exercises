fun1 f p xs = [f x | x <- xs, p x ]

fun2 f p = map f . filter p

-- *Main> fun1 (*2) (>2) [1,7,8]
-- [14,16]
-- *Main> fun2 (*2) (>2) [1,7,8]
-- [14,16]
