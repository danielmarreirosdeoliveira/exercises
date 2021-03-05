--import Prelude(Functor,fmap,Int,(^),(+),Show,Bool(True,False),not)

inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

map' :: (a -> b) -> [] a -> [] b
-- map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

inc' = map' (+1)
sqr' = map' (^2)

class Functor' f where
   fmap :: (a -> b) -> f a -> f b

instance Functor' [] where
   -- fmap :: (a -> b) -> [a] -> [b]
   fmap = map'

data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
   -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
   fmap _ Nothing'  = Nothing'
   fmap g (Just' a) = Just' (g a)

-- ghci> Prelude.fmap (+1) Nothing'
-- Nothing'
-- ghci> Prelude.fmap (+1) (Just' 2)
-- Just' 3
-- ghci> Prelude.fmap not (Just' False)
-- Just' True

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
   -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap g (Leaf x)   = Leaf (g x)
   fmap g (Node l r) = Node (Prelude.fmap g l) (Prelude.fmap g r)

-- ghci> Prelude.fmap (+1) (Node (Leaf 1) (Leaf 2))
-- Node (Leaf 2) (Leaf 3)




-- instance Functor IO where
   -- fmap :: (a -> b) -> IO a -> IO b
--    fmap g mx = do {x <- mx; return (g x)}
--
-- ghci> Prelude.fmap show (return True)
-- "True"

