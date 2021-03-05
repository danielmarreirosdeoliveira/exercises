last' :: [a] -> a
last' xs = head (reverse xs)


last'' :: [a] -> a
last'' xs  = head (drop (length xs - 1) xs)
