-- putStr' :: String -> IO ()
-- putStr' []     = return ()
-- putStr' (x:xs) = do putChar x
--                     putStr' xs



putStr' :: String -> IO ()
putStr' xs = sequence_ [(putChar x) | x <- xs]