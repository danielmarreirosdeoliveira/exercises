


init0 :: [a] -> [a]
init0 [_] = []
init0 (x:xs) = x : init0 xs