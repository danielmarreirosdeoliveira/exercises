-- THE TWO FUNCTOR LAWS
-- 1. fmap id      = id
-- 2. fmap (g . h) = fmap g . fmap h

class Functor' f where
   fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
   -- fmap' :: (a -> b) -> f a -> f b
   fmap' g []     = []
   fmap' g (x:xs) = fmap g xs ++ [g x]


-- *Main> fmap' id [1,2]
-- [2,1]
-- this clearly violates the first functor law
-- since
-- *Main> id [1,2]
-- [1,2]

-- *Main> fmap' (not . even) [1,2]
-- [False,True]
-- this violates the second functor law
-- since
-- *Main> (fmap' not . fmap' even) [1,2]
-- [True,False]

