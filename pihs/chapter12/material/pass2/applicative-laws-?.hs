-- pure id <*>     = id
-- pure (g x)      = pure g <*> pure x
-- x <*> pure y    = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z



-- also satisfied equation:
-- fmap g x = pure g <*> x

-- with g <$> x = fmap g x we can write
-- g <$> x1 <*> x2 <*> ... <*> xn