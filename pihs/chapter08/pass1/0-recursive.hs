data Nat = Zero | Succ Nat deriving Show

a :: Nat
a = Zero

b :: Nat
b = Succ (Succ Zero)

c :: Nat
c = Succ Zero


nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

addNats0 :: Nat -> Nat -> Nat
addNats0 x y = int2nat (nat2int x + nat2int y)

addNats1 :: Nat -> Nat -> Nat
addNats1 Zero     n = n
addNats1 (Succ n) m = Succ (addNats1 n m)




----

data List a = Nil | Cons a (List a) deriving Show

d :: List Int
d = Nil

e :: List Int
e = Cons 3 Nil

f :: List Int
f = Cons 3 (Cons 4 Nil)

len0 :: List a -> Int
len0 Nil         = 0
len0 (Cons x xs) = 1 + len0 xs


---

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- If applying this function to a tree gives a sorted list, then the tree is called a search tree.

-- so a more efficient version for search trees of occurs, which traverses only half the tree, can be given

occurs0 :: Ord a => a -> Tree a -> Bool
occurs0 x (Leaf y)                 = x == y
occurs0 x (Node l y r) | x == y    = True
                       | x < y     = occurs0 x l
                       | otherwise = occurs0 x r

----

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)

data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)

data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)

data Tree4 a = Node4 a [Tree4 a]











