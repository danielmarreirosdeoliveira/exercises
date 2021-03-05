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


go' :: Int -> IO ()
go' n = do s <- sequence [x | x <- (replicate n (getDigit ""))]
           putStr "The total is "
           putStrLn (show (sum s))
           return ()



adder :: IO ()
adder = do putStr "How many numbers? "
           r <- getDigit ""
           go' r
           return ()





-- with numbers instead of digits

getNumber :: IO Int
getNumber =
   do n <- getLine
      return (read n)

go'' :: Int -> IO ()
go'' n = do s <- sequence [x | x <- (replicate n (getNumber))]
            putStr "The total is "
            putStrLn (show (sum s))
            return ()

adder' :: IO ()
adder' = do putStr "How many numbers? "
            r <- getDigit ""
            go'' r
            return ()