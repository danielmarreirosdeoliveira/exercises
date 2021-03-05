curry0 :: ((a,b) -> c) -> (a -> b -> c)
curry0 f = \x -> \y -> f(x,y)

uncurry0 :: (a -> b -> c) -> ((a,b) -> c)
uncurry0 f = \(x,y) -> f x y