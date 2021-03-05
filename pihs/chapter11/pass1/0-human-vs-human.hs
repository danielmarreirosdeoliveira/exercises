import TicTacToe

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p


run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!"
         | wins X g  = putStrLn "Player X wins!"
         | full g    = putStrLn "It's a draw!"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    []   -> do putStrLn "ERROR: Invalid move"
                               run' g p
                    [g'] -> run g' (next p)













