module Grid where

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)


next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g

-- GENERALIZED WITH TYPE PARAMETER
-- diag :: Grid -> [Player]
diag :: [[a]] -> [a]
diag g = [g !! n !! n | n <- [0..size-1]]

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g