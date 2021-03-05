import System.IO
import Data.Char







getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x



readChars :: IO String
readChars = do
   x <- getCh
   case x of
     '\n'   -> do putChar '\n'
                  return []
     '\DEL' -> do putChar '\b'
                  putChar ' '
                  putChar '\b'
                  xs <- readChars
                  return (x:xs)
     _      -> do putChar x
                  xs <- readChars
                  return (x:xs)

cleanup :: String -> String
cleanup = foldl (\xs x -> if x == '\DEL' then init xs else xs ++ [x]) ""

readLine :: IO String
readLine = do xs <- readChars
              return (cleanup xs)