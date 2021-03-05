data Rapper = R { firstName :: String
                , flavor :: String
                } deriving (Show)

-- *Main> :t flavor
-- flavor :: Rapper -> String
-- *Main> flav = R { firstName = "abc", flavor = "Strawberry" }
-- *Main> flavor flav
-- "Strawberry"


newtype Rapper' = R' { rap :: String } deriving Show -- the constructor of a newtype can have exactly one field
-- *Main> r = R' "yo"
-- *Main> rap r
-- "yo"
