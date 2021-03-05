data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show
-- *Main> :t Leaf
-- Leaf :: a -> Tree a
-- *Main> :t Node
-- Node :: Tree a -> Tree a -> Tree a

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _)   n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                         (l',n')  = rlabel l n
                         (r',n'') = rlabel r n'

-- *Main> rlabel tree 0
-- (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2),3)
-- *Main> fst $ rlabel tree 0
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)


type State = Int
newtype ST a = ST (State -> (a,State))
app :: ST a -> State -> (a,State)
app (ST st) s = st s

fresh :: ST Int
fresh = ST (\n -> (n, n+1))

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = ST (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = ST (\s -> (x,s))
   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = ST (\s ->
      let (f,s')  = app stf s
          (x,s'') = app stx s' in (f x, s''))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- *Main> :t pure Leaf
-- pure Leaf :: Applicative f => f (a -> Tree a)
-- :t pure Node
-- pure Node :: Applicative f => f (Tree a -> Tree a -> Tree a)

-- *Main> :t alabel (Leaf 'a')
-- alabel (Leaf 'a') :: ST (Tree Int)

-- *Main> app (alabel tree) 0
-- (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2),3)

instance Monad ST where
   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = ST (\s -> let (x,s') = app st s in app (f x) s')

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

-- *Main> app (mlabel tree) 0
-- (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2),3)
