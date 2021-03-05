data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- *Main> :t flavor
-- flavor :: Person -> String

-- *Main> p = Person { firstName = "abc", lastName = "def", age = 28, height = 180.0, phoneNumber = "0800", flavor = "Strawberry" }
-- *Main> p
-- Person {firstName = "abc", lastName = "def", age = 28, height = 180.0, phoneNumber = "0800", flavor = "Strawberry"}

-- flav = p
-- *Main> flavor flav
-- "Strawberry"

data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c} deriving (Show)

-- *Main> Car "A" "B" "C"
-- Car {company = "A", model = "B", year = "C"}

