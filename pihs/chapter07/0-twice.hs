twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- *Main> twice (\x -> x * 2) 2
-- 8

-- *Main> twice reverse [1,2,3]
-- [1,2,3]

quadruple = twice (*2)
