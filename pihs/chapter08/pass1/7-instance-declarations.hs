data Maybe0 a = Just0 a | Nothing0

instance Eq a => Eq (Maybe0 a) where
    Nothing0 == Nothing0 = True
    Just0 a == Just0 b   = a == b
    _ == _               = False



-- *Main> Just0 7 == Just0 8
-- False
-- *Main> Just0 7 == Just0 7
-- True
-- *Main> Just0 7 == Nothing0
-- False
-- *Main> Nothing0  == Just0 7
-- False


-------


-- instance Eq a => Eq [a] where
--     []     == []     = True
--     []     == _      = False
--     _      == []     = True
--     (x:xs) == (y:ys) = x == y && xs == ys

data List' a = Empty' | a :-: (List' a) deriving Show
infixr 5 :-: -- right associative, the number is the precedence, where function application has precedence 10

instance Eq a => Eq (List' a) where
    Empty'     == Empty'    = True
    Empty'     == _         = False
    _          == Empty'    = False
    (x :-: xs) == (y :-: ys) = x == y && xs == ys


-- *Main> (6 :-: 7 :-: Empty') == (6 :-: 8 :-: Empty')
-- False
-- *Main> (6 :-: 7 :-: Empty') == (6 :-: 7 :-: Empty')
-- True


