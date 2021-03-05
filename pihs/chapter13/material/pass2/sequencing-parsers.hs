import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                       []    -> []
                       (x:xs) -> [(x,xs)])

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

-- > parse (fmap toUpper item) "abc"
-- [('A',"bc")]

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

-- > parse (toUpper <$> item) "abc"
-- [('A',"bc")]
-- > parse (pure 1) "abc"
-- [(1,"abc")]

three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x, z)

-- *Main> parse three "abcde"
-- [(('a','c'),"de")]
-- *Main> parse three "ab"
-- []

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

three' :: Parser (Char,Char)
three' = do x <- item
            item
            z <- item
            return (x,z)

-- > parse three' "abcde"
-- [(('a','c'),"de")]
-- > parse three' "ab"
-- []

three'' :: Parser (Char,Char)
three''  = item >>= \x ->
           item >>= \y ->
           item >>= \z ->
           return (x,z)

-- > parse three'' "abcde"
-- [(('a','c'),"de")]
-- > parse three'' "ab"
-- []








