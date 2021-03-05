import Prelude(Eq, Ord, Show, Read, (==), (>=), (/=), (<), show, read, Float)


data Bool0 = False0 | True0
             deriving (Eq, Ord, Show, Read)

t :: Bool0
t = True0

f :: Bool0
f = False0




-- depeding on the order of definition of data constructors (False0 | True0 vs. True0 | False0)
--
-- [1 of 1] Compiling Main             ( 0-derived.hs, interpreted )
-- Ok, one module loaded.
-- *Main> False0 >= True0
-- True
-- *Main> :r
-- [1 of 1] Compiling Main             ( 0-derived.hs, interpreted )
-- Ok, one module loaded.
-- *Main> False0 >= True0
-- False





-- in case of constructors with arguments, the types of these arguments must also be instances of these derived classes

data Shape = Circle Float | Rect Float Float deriving (Eq, Ord)

data Maybe a = Nothing | Just a deriving Eq



s1 = Circle 10
s2 = Circle 10
s3 = Circle 11
s4 = Rect 3 4
s5 = Rect 3 4
s6 = Rect 4 5
s7 = Rect 5 4


m1 = Nothing
m2 = Nothing
m3 = Just 2
m4 = Just 2
m5 = Just 3