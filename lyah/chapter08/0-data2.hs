import Prelude hiding (Bool, True, False)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- *Main> surface (Rectangle (Point 0 0) (Point 100 100))
-- 10000.0
-- *Main> surface (Circle (Point 0 0) 24)
-- 1809.5574


