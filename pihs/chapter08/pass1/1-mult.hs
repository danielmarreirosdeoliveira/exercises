data Nat = Zero | Succ Nat deriving Show

a :: Nat
a = Zero

b :: Nat
b = Succ (Succ Zero)

c :: Nat
c = Succ Zero


addNats :: Nat -> Nat -> Nat
addNats Zero     n = n
addNats (Succ n) m = Succ (addNats n m)

multNats :: Nat -> Nat -> Nat
multNats m Zero     = Zero
multNats m (Succ n) = addNats m (multNats m n)


