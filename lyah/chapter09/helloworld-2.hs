main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- ghc --make helloworld-2
-- ./helloworld-2

-- > main
-- Hello, what's your name?