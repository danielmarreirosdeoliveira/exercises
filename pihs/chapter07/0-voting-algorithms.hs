import Data.List


-- First past the post --

count0 :: Eq a => a -> [a] -> Int
count0 x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count0 v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- *Main> winner ["Red","Green","Blue","Green","Blue","Green","Blue","Blue"]
-- "Blue"




-- Alternative vote --

ballots1 :: [[String]]
ballots1 = [["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner1 :: Ord a => [[a]] -> a
winner1 bs = case rank (rmempty bs) of
             [c]    ->  c
             (c:cs) -> winner1 (elim c bs)

-- *Main> winner1 ballots1
-- "Green"
