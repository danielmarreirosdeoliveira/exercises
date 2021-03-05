import System.IO
import Data.List
import TicTacToe


data Tree a = Node a [Tree a]
              deriving Show

instance Eq (Tree a) where
   _ == _ = True

instance Ord (Tree a) where
   compare _ _ = EQ


moves :: Grid -> Player -> [Grid]
moves g p
   | won g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]



gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p)| g' <- moves g p]



prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


d :: Int
d = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']


play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p


bestmoves' :: Grid -> Player -> [Tree (Grid,Player)]
bestmoves' g p = [t | t@(Node (g',p') _) <- ts, p' == best]
                 where
                    tree = prune d (gametree g p)
                    Node (_,best) ts = minimax tree


play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn "Player O wins!"
   | wins X g = putStrLn "Player X wins!"
   | full g   = putStrLn "It's a draw!"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      [] -> do putStrLn "ERROR: Invalid move"
                               play' g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking... "
                   let ts = bestmoves' g p
                   let (Node (bestmove,_) _) = snd (head (sort [(depth t, t) | t <- ts]))
                   (play $! bestmove) (next p)




depth :: Tree a -> Int
depth (Node _ []) = 0
depth (Node _ ts) = 1 + maximum (map depth ts)




main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O