-- > safetail [3,4,7]
-- [4,7]
-- > safetail [3,4]
-- [4]
-- > safetail [2]
-- []
-- > safetail []
-- []

safetail0 :: [a] -> [a]
safetail0 xs = if null xs then [] else tail xs

safetail1 :: [a] -> [a]
safetail1 xs | null xs = []
             | otherwise = tail xs

safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 xs = tail xs