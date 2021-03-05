module Input where

import Data.Char
import Grid

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "