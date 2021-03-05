
type State = Int

-- StateTransformer
-- type ST = State -> (a,State)
newtype ST a = ST (State -> (a,State))
-- we need a dummy constructor and the app function for our ST to be parametrizable
app :: ST a -> State -> (a,State)
app (ST st) s = st s

-- *Main> app (ST (\s -> (3, s))) 4
-- (3,4)

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = ST (\s -> let (x,s') = app st s in (g x, s'))

-- *Main> st = ST (\s -> (3*s, s+1))
-- *Main> app (fmap (*2) st) 4
-- (24,5)

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = ST (\s -> (x,s))

   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = ST (\s ->
      let (f,s')  = app stf s
          (x,s'') = app stx s' in (f x, s''))

-- stf = ST (\s -> ((3*), s+1))
-- stx = ST (\s -> (s*2, s+1))
-- *Main> app (stf <*> stx) 4
-- (30,6)

instance Monad ST where
   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = ST (\s -> let (x,s') = app st s in app (f x) s')

