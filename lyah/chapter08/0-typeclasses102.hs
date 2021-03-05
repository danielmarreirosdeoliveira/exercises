import Prelude hiding (Eq, (==), (/=))

class Eq a where
   (==) :: a -> a -> Bool
   (/=) :: a -> a -> Bool
   x == y = not (x /= y)
   x /= y = not (x == y)

-- *Main> :t (==)
-- (==) :: Eq a => a -> a -> Bool

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
   Red == Red = True
   Yellow == Yellow = True
   Green == Green = True
   _ == _ = False

-- *Main> Red == Red
-- True
-- *Main> Red == Yellow
-- False

instance Show TrafficLight where
   show Red = "Red light"
   show Yellow = "Yellow light"
   show Green = "Green light"

-- *Main> [Red, Yellow, Green]
-- [Red light,Yellow light,Green light]