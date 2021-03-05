import Prelude hiding (Functor, fmap)

class Functor f where
   fmap :: (a -> b) -> f a -> f b

instance Functor ((->) r) where
    -- fmap :: (a -> b) -> ((->) a) -> ((->) b)

    -- fmap f g = (\x -> f (g x))
    fmap = (.)

-- meaning:
-- instance Functor (r ->) where
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)

-- fmap (+1) (*2) $ 5
-- 11

--             (r->b                  )
--             (a->b)      (r->a)     r
--             (Int->Bool) (Int->Int) Int
-- *Main> fmap even        (*2)     $ 5
-- True

-- *Main> f = fmap  even   (*2  )
--                 (a->b)  (r->a)
--                 (r->b        )
-- *Main> f 5
-- True

-- *Main> (*2) `fmap` (+100) $ 4
-- 208
-- *Main> (*2) . (+100) $ 4
-- 208


-- lifting
-- fmap :: (a -> b) -> (f a -> f b)

-- *Main> :t fmap (*2)
-- fmap (*2) :: (Num b, Functor f) => f b -> f b
-- *Main> :t fmap (replicate 3)
-- fmap (replicate 3) :: Functor f => f a -> f [a]

-- *Main> :t fmap even
-- fmap even :: (Integral a, Functor f) => f a -> f Bool
-- *Main> :t (2*)
-- (2*) :: Num a => a -> a
