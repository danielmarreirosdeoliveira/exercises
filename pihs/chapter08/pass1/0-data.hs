data Bool = False | True -- '|' is read as or. The values are called constructors.

-- data Bool2 = False | True -- the constructors cannot be 'reused'.

----

type Pos = (Int, Int)
data Move = North | East | South | West deriving Show

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move East  (x, y) = (x + 1, y)
move South (x, y) = (x, y - 1)
move West  (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves []     p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East

----

data Shape = Circle Float | Rect Float Float deriving Show
-- because of their arguments, Circle and Rect are actually constructor functions
-- *Main> :type Circle
-- Circle :: Float -> Shape


square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

----

data Maybe0 a = Nothing0 | Just0 a deriving Show
ddd :: Maybe0 Int
ddd = Just0 3

safediv :: Int -> Int -> Maybe0 Int
safediv _ 0 = Nothing0
safediv m n = Just0 (m `div` n)

safehead :: [a] -> Maybe0 a
safehead [] = Nothing0
safehead xs = Just0 (head xs)