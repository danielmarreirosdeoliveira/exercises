import TicTacToe

data Tree a = Node a [Tree a]
              deriving Show


moves :: Grid -> Player -> [Grid]
moves g p
   | won g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]



gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p)| g' <- moves g p]



count :: Tree a -> Int
count (Node _ ts) = 1 + sum [count t | t <- ts]

-- ghci> count (gametree empty O)
-- 549946



depth :: Tree a -> Int
depth (Node _ []) = 0
depth (Node _ ts) = 1 + maximum (map depth ts)
