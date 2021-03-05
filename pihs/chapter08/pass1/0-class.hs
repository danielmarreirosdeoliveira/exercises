import Prelude(Bool, Bool(True, False), otherwise, Show)

data Bool0 = True0 | False0 deriving Show

class Eq0 a where
--     (==), (/=) :: a -> a -> Bool0
    (==) :: a -> a -> Bool0



instance Eq0 Bool0 where
    False0 == False0  = True0
    True0  == True0   = True0
    _      == _       = False0


convert :: Bool0 -> Bool
convert False0 = False
convert True0  = True


same :: Eq0 a => a -> a-> Bool0
same x y = x Main.== y


(||) :: Bool0 -> Bool0 -> Bool0
False0 || False0 = False0
_ || _           = True0
--

class Eq0 a => Ord0 a where
    (<), (<=), (>), (>=) :: a -> a -> Bool0
    min, max             :: a -> a -> a

    min x y | convert(x <= y)  = x
            | otherwise        = y

    max x y | convert(x <= y)  = y
            | otherwise        = x


instance Ord0 Bool0 where
    False0 < True0  = True0
    _      < _      = False0

    b <= c = (b < c) || (b == c)
    b > c  = c < b
    b >= c = c <= b

