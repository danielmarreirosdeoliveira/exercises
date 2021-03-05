module Move where

import Grid

-- positions:
-- 0 1 2
-- 3 4 5
-- 6 7 8

-- returns True if the position is still free (i.e. B)
valid :: Grid -> Int -> Bool
valid g position = 0 <= position && position < size^2 && concat g !! position == B

-- Singleton list is success, empty list is failure
move :: Grid -> Int -> Player -> [Grid]
move g position p =
   if valid g position then [chop size (xs ++ [p] ++ ys)] else []
   where (xs,B:ys) = splitAt position (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)