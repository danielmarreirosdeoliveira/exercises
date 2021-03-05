map' :: (a->b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

class Functor' f where
   fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
   -- fmap' :: (a -> b) -> [a] -> [b]
   fmap' = map'

-- example:
-- *Main> fmap' (+1) [1,2,3]
-- [2,3,4]

data Maybe' a = Nothing' | Just' a
   deriving Show

instance Functor' Maybe' where
   -- fmap' :: (a -> b) -> Maybe a -> Maybe b
   fmap' _ Nothing'  = Nothing'
   fmap' g (Just' x) = Just' (g x)

-- examples:
-- *Main> a = Just' 1
-- *Main> fmap' (+1) a
-- Just' 2
-- *Main> fmap' (+1) Nothing'
-- Nothing'
-- *Main> fmap' not (Just' False)
-- Just' True



-- GENERALIZED INC FUNCTION, WORKS WITH EVERY FUNCTOR TAKING Int!
inc :: Functor' f => f Int -> f Int
inc = fmap' (+1)

-- examples:
-- *Main> inc (Just' 1)
-- Just' 2
-- *Main> inc [1,2,3]
-- [2,3,4]

-- THE TWO FUNCTOR LAWS
-- 1. fmap id      = id
-- 2. fmap (g . h) = fmap g . fmap h










