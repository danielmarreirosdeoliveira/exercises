import System.IO
import Data.List
import TicTacToe

-- part a of exercise is solved


data Tree a = Node a [Tree a]
              deriving Show


moves :: Grid -> Player -> [Grid]
moves g p
   | won' g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]



gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p)| g' <- moves g p]



prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


depth :: Int
depth = 9


-- Ex. 4a
-- we use an additional param instead of the turn method
-- otherwise there is a bug where i can if i let X begin

minimax :: Player -> Tree Grid -> Tree (Grid, Player)
minimax p (Node g [])
   | wins' O g  = Node (g,O) []
   | wins' X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax p (Node g ts)
   | p == O = Node (g, minimum ps) ts'
   | p == X = Node (g, maximum ps) ts'
              where
                 ts' = map (minimax (next p)) ts
                 ps  = [p | Node (_,p) _ <- ts']


-- Ex. 4c
-- p' is now the precalculated minimum or maximum value for which we find the first occurrence in the childrens list
-- sidenote: we do no pruning anymore
bestmove :: Tree (Grid, Player) -> Tree (Grid, Player)
bestmove (Node (_,p') ts) = head [t' | t'@(Node (_, p'') _) <- ts, p'' == p']



play :: Tree (Grid, Player) -> Player -> IO ()
play t@(Node (g,_) _) p = do cls
                             goto (1,1)
                             putGrid g
                             play' t p


play' :: Tree (Grid, Player) -> Player -> IO ()
play' t@(Node (g,_) ts) p
   | wins' O g = putStrLn "Player O wins!"
   | wins' X g = putStrLn "Player X wins!"
   | full g   = putStrLn "It's a draw!"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      [] -> do putStrLn "ERROR: Invalid move"
                               play' t p
                      [g'] -> play t_for_g (next p)
                              where t_for_g = head (filter (\t'' -> case t'' of (Node (g'',_) _) -> g'' == g') ts) -- branch which matches the given grid
   | p == X   = do putStr "Player X is thinking... "
                   (play $! (bestmove t)) (next p)





main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Hello, choose if you want to play first or second (1,2)"
          p <- getNat " "
          case p of                                            -- Ex. 4a
               1 -> play (minimax O (gametree empty O)) O
               2 -> play (minimax X (gametree empty X)) X
               _ -> main




-- Ex. 4b
-- see also size in TicTacToe.hs
winningLineLength :: Int
winningLineLength = 3

wins' :: Player -> Grid -> Bool
wins' p g = any line (rows ++ cols ++ dias)
           where
              line = (== winningLineLength) . length . filter (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

won' :: Grid -> Bool
won' g = wins' O g || wins' X g


