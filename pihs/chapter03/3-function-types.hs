second :: [a] -> a
second xs = head (tail xs)

-- swap :: (a, a) -> (a, a) -- also possible, but unnecessarily specific
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

-- palindrome :: String -> Bool
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)