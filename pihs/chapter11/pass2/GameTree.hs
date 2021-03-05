module GameTree where

import Grid
import Move

data Tree a = Node a [Tree a]
              deriving Show

moves :: Grid -> Player -> [Grid]
moves g p
   | won g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

depth :: Int
depth = 9

-- this is the sample for which the tree is depicted on page 146
sample :: Grid
sample = [[O,B,B],[X,X,O],[X,O,B]]