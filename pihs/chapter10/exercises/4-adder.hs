import Data.Char



newline :: IO ()
newline = putChar '\n'



getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt



go :: Int -> Int -> IO ()
go total remaining | remaining == 0
                   = do putStr "The total is "
                        putStrLn (show total)
                        return ()
                   | otherwise
                   = do x <- getDigit ""
                        go (total+x) (remaining-1)
                        return ()




adder :: IO ()
adder = do putStr "How many numbers? "
           r <- getDigit ""
           go 0 r
           return ()

