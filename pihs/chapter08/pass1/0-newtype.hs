newtype Nat = N Int deriving Show


a = N 3


-- 'newtype' is not only a synonym as 'type' is, so types cannot be mixed up
-- using newtype rather than 'data' is efficient because compiler removes it after type checking

