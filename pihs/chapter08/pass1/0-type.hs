type Pos = (Int, Int)
type Trans = Pos -> Pos
-- type Tree = (Int, [Tree]) -- not permitted in Haskell because it is recursive


type Pair a = (a, a) -- a parametrize type
cde :: Pair Int
cde = (3, 3)


type Assoc k v = [(k, v)]

abc :: Assoc Int Int
abc = [(1,7),(9,11)]


find0 :: Eq k => k -> Assoc k v -> v
find0 k t = head [v | (k', v) <- t, k == k']