import System.IO
import GameTree
import Grid
import Input
import Display
import Move



moves :: Grid -> Player -> [Grid]
moves g p
   | won g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]





-- this is still melting my brain and
-- unfortunately does not work.
-- for now I'm quiet happy that I got the types right.

maximize :: (Maybe (Tree (Grid,Player)), Player, Player,  Bool)
            -> Tree Grid
            -> (Maybe (Tree (Grid, Player)), Player, Player, Bool)

maximize (mn, a, b, True) _                     = (mn, a, b, True)
maximize (Nothing, a, b, False) n               = (Just (abprune a b n), a, b, False)
maximize (Just n'@(Node (_, p') _), a, b, False) n
   | p'' <= b  = (Just n', a, b, True)
   | p'' > p'  = (Just n'', p'', b, False)
   | otherwise = (Just n', a, b, False)
                 where n''@(Node (_, p'') _) = abprune a b n

minimize :: (Maybe (Tree (Grid,Player)), Player, Player,  Bool)
            -> Tree Grid
            -> (Maybe (Tree (Grid, Player)), Player, Player, Bool)

minimize (mn, a, b, True) _                     = (mn, a, b, True)
minimize (Nothing, a, b, False) n               = (Just (abprune a b n), a, b, False)
minimize (Just n'@(Node (_, p') _), a, b, False) n
   | p'' >= a  = (Just n', a, b, True)
   | p'' < p'  = (Just n'', a, p'', False)
   | otherwise = (Just n', a, b, False)
                 where n''@(Node (_, p'') _) = abprune a b n

abprune :: Player -> Player -> Tree Grid -> Tree (Grid, Player)
abprune _ _ (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
abprune a b (Node g ts)
   | turn g == O = case (foldl minimize (Nothing, a, b, False) ts) of
                   (Just n, _, _, _) -> n
                   (Nothing, _, _, _) -> Node (g,O) [] -- happens never
   | turn g == X = case (foldl maximize (Nothing, a, b, False) ts) of
                   (Just n, _, _, _) -> n
                   (Nothing, _, _, _) -> Node (g,X) [] -- happens never


bestmove :: Grid -> Player -> Grid
bestmove g p = g' where Node (g', p) ts = abprune O X (gametree g p)


play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

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
                   (play $! (bestmove g p)) (next p)





main :: IO ()
main = do hSetBuffering stdout NoBuffering
          -- play empty O
          play sample O
