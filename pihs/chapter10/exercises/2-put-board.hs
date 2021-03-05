

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]


putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))




putBoard :: Board -> IO ()
putBoard = putBoard' 1

putBoard' :: Int -> Board -> IO ()
putBoard' _ []     = return ()
putBoard' i (x:xs) = do putRow i x
                        putBoard' (i+1) xs


