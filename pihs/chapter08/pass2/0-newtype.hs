newtype Nat = N Int -- natural numbers

-- newtype has, unlike data, a single type constructor

-- how does it compare to:
-- type Nat = Int   <- here the types are synonyms, wheras with newtype you have an entirely new type
-- data Nat = N Int <- newtype is more efficient than this version here
-- ?

