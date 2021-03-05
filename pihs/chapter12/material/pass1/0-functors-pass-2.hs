inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

map' :: (a->b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

inc' = map' (+1)

sqr' = map' (^2)

class Functor' f where
   fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
   -- fmap :: (a -> b) -> [a] -> [b]
   fmap' = map'




data Maybe' a = Nothing' | Just' a deriving Show

instance Functor' Maybe' where
   -- fmap :: (a -> b) -> Maybe a -> Maybe b
   fmap' _ Nothing'  = Nothing'
   fmap' g (Just' x) = Just' (g x)


--ghci> fmap' (+1) Nothing'
--Nothing'
--ghci> fmap' (*2) (Just' 3)
--Just' 6
--ghci> fmap' not (Just' False)
--Just' True


data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor' Tree where
   -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap' g (Leaf x)   = Leaf (g x)
   fmap' g (Node l r) = Node (fmap' g l) (fmap' g r)

--ghci> fmap' length (Node (Leaf "abc") (Leaf "cdef"))
--Node (Leaf 3) (Leaf 4)


instance Functor' IO where
   -- fmap :: (a -> b) -> IO a -> IO b
   fmap' g mx = do {x <- mx; return (g x)}

--ghci> fmap' show (return True)
--"True"




inc'' :: Functor' f => f Int -> f Int
inc'' = fmap' (+1)

--ghci> inc'' (Just' 1)
--Just' 2
--ghci> inc'' [1,2,3]
--[2,3,4]
--ghci> inc'' (Node (Leaf 1) (Leaf 2))
--Node (Leaf 2) (Leaf 3)


-- Equational laws
-- fmap id      = id
-- fmap (g . h) = fmap g . fmap h




class Functor'' f where
   fmap'' :: (a -> b) -> f a -> f b

instance Functor'' [] where      -- fails to satisfy functor laws
   -- fmap :: (a -> b) -> f a -> f b
   fmap'' g' []     = []
   fmap'' g (x:xs) = fmap'' g xs ++ [g x]


--ghci> fmap'' id [1,2]
--[2,1]
--ghci> id [1,2]
--[1,2]
--ghci> fmap'' (not . even) [1,2]
--[False,True]
--ghci> (fmap'' not . fmap'' even) [1,2]
--[True,False]






