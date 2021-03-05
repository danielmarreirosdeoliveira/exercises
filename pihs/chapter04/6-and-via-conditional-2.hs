-- True && b = b
-- False && _ = False

(&&&) :: Bool -> Bool -> Bool
b &&& c = if b then c else False