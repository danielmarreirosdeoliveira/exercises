-- True && True = True
-- _    && _    = False

(&&&) :: Bool -> Bool -> Bool
b &&& c = if b then (if c then True else False) else False