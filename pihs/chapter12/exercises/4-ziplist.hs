newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
   -- fmap :: (a -> b) -> ZipList a -> ZipList b
   fmap g (Z xs) = Z (map g xs)

-- > fmap (+3) $ Z [1, 17]
-- Z [4,20]

instance Applicative ZipList where
   -- pure :: a -> ZipList a
   pure = Z . repeat

   -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
   (Z fs) <*> (Z xs) = Z $ map (\(f,x) -> f x) (zip fs xs)

-- > pure (*3) <*> Z [1,10]
-- Z [3,30]

-- solution from the book
   -- pure x = Z (repeat x)
   -- (Z fs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]