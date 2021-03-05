third0 :: [a] -> a
third0 xs = head (tail (tail xs))

third1 :: [a] -> a
third1 xs = xs !! 2

third2 :: [a] -> a
third2 (_ : (_ : (a : _))) = a