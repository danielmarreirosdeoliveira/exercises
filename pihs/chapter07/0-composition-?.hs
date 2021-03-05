

(.!) :: (b -> c) -> (a -> b) -> (a -> c)
f .! g = \x -> f (g x)
-- (f .! g) x = f (g x)


-- odd n = not (even n)
odd0 = not .! even

-- twice f x = f (f x)
twice0 f = f .! f

sumsqreven = sum . map (^2) . filter even






id0 :: a -> a
id0 = \x -> x


compose0 :: [a -> a] -> (a -> a)
compose0 = foldr (.) id



-- sumsqreven1 = compose0 [even, filter, map (^2), sum]
-- here there is more necessary to make this work when types change
