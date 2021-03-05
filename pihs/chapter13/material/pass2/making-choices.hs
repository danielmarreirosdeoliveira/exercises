import Control.Applicative hiding (Alternative,(<|>),empty)
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                       []    -> []
                       (x:xs) -> [(x,xs)])

instance Functor Parser where
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   pure v = P (\inp -> [(v,inp)])
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

class Applicative f => Alternative f where
   empty :: f a
   (<|>) :: f a -> f a -> f a

instance Alternative Maybe where
   -- empty :: Maybe a
   empty = Nothing

   -- (<|>) :: Maybe a -> Maybe a -> Maybe a
   Nothing  <|> my = my
   (Just x) <|> _  = Just x

-- > Nothing <|> Just 3
-- Just 3
-- > Just 3 <|> Nothing
-- Just 3
-- > Just 3 <|> Just 6
-- Just 3
-- > Nothing <|> Nothing
-- Nothing

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- > parse (item <|> pure 'd') "a"
-- [('a',"")]
-- > parse (item <|> pure 'd') ""
-- [('d',"")]
-- > parse (empty <|> pure 'd') "abc"
-- [('d',"abc")]
-- > parse (item <|> item) ""
-- []
-- > parse (item <|> item) "a"
-- [('a',"")]
-- > parse (empty <|> empty) "a"
-- []




