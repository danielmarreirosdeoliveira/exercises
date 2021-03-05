altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []     = []
altMap f g (x:[]) = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs