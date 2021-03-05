-- > halve [1,2,3,4,5,6]
-- ([1,2,3], [4,5,6])

halve :: [a] -> ([a], [a])
halve xs = (take h xs, drop h xs)
  where
    h = (length xs) `div` 2


