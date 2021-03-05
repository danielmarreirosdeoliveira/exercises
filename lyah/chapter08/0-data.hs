import Prelude hiding (Bool, True, False)

data Bool = True | False
   deriving Show

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
   deriving Show

-- *Main> :t Circle
-- Circle :: Float -> Float -> Float -> Shape

surface :: Shape -> Float
surface (Circle _ _ r)          = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- *Main> surface $ Circle 10 20 10
-- 314.15927
-- *Main> surface $ Rectangle 0 0 100 100
-- 10000.0

-- *Main> map (Circle 10 20) [4,5,6,6]
-- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]