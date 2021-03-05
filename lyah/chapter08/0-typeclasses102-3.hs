import Prelude hiding (Maybe, Just, Nothing)

data Maybe a = Just a | Nothing

instance (Eq m) => Eq (Maybe m) where
   Just x == Just y = x == y
   Nothing == Nothing = True
   _ == _ = False

-- *Main> Just 3 == Just 3
-- True
-- *Main> Just 3 == Just 4
-- False

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
   Red == Red = True
   Yellow == Yellow = True
   Green == Green = True
   _ == _ = False

-- *Main> Just Red == Just Red
-- True
-- *Main> Just Red == Just Green
-- False



