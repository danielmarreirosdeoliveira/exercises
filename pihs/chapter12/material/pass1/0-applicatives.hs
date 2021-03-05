class Functor' f where
   fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
   -- fmap' :: (a -> b) -> Maybe' a -> Maybe' b
   fmap' _ Nothing  = Nothing
   fmap' g (Just a) = Just (g a)

class Functor' f => Applicative' f where
   pure'  :: a -> f a
   (<*>!) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
   -- pure' :: a -> Maybe a
   pure' = Just
   -- (<*>!) :: Maybe (a -> b) -> Maybe a -> Maybe b
   Nothing  <*>! _  = Nothing
   (Just g) <*>! mx = fmap' g mx

--ghci> Just (+1) <*>! Just 3
--Just 4
--ghci> Nothing <*>! Just 3
--Nothing
--ghci> pure' (+1) <*>! Just 1
--Just 2
--ghci> pure' (+1) <*>! Nothing
--Nothing
--ghci> pure' (+) <*>! Just 1 <*>! Just 2
--Just 3
--ghci> pure' (+) <*>! Nothing <*>! Just 2
--Nothing
--ghci> pure' (+) <*>! Just 2 <*>! Nothing
--Nothing

instance Functor' [] where
   -- fmap' :: (a -> b) -> [a] -> [b]
   fmap' = map

instance Applicative' [] where
   -- pure :: a -> [a]
   pure' x = [x]
   -- (<*>!) :: [a -> b] -> [a] -> [b]
   gs <*>! xs = [g x | g <- gs, x <- xs]

-- ghci> pure' (*) <*>! [1,2] <*>! [3,4]
-- [3,4,6,8]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*>! xs <*>! ys

fmap2 :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure' g <*>! x <*>! y

prods'' xs ys = fmap2 (*) xs ys

--ghci> prods [1,2] [3,4]
--[3,4,6,8]
--ghci> prods' [1,2] [3,4]
--[3,4,6,8]
--ghci> prods'' [1,2] [3,4]
--[3,4,6,8]

instance Functor' IO where
  fmap' f x           =  x >>= (return . f) -- taken from prelude

instance Applicative' IO where
  -- pure' :: a -> IO a
  pure' = return

  -- (<*>!) :: IO (a -> b) -> IO a -> IO b
  mg <*>! mx = do {g <- mg; x <- mx; return (g x)}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*>! getChar <*> getChars (n-1)

sequenceA' :: Applicative' f => [f a] -> f [a]
sequenceA' [] = pure' []
sequenceA' (x:xs) = pure' (:) <*>! x <*>! sequenceA' xs

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

--ghci> getChars 4
--abcd"abcd"
--ghci> getChars' 4
--abcd"abcd"



-- Equational laws

-- pure id <*> x  = x
-- pure (g x)      = pure g <*>! pure x
-- x <*> pure y   = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z


-- one extra, which comes for free
-- fmap g x = pure g <*> x

-- applicative style
-- pure g <*> x1 <*> x2 <*> ... <*> xn
-- or
-- g <&> x1 <*> x2 <*> ... <*> xn




