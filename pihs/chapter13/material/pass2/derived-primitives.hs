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

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])



sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- > parse (char 'a') "abc"
-- [('a',"bc")]
-- > parse (char 'a') "dbc"
-- []

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- > parse (string "d") "dbc"
-- [("d","bc")]
-- > parse (string "dbc") "dbc"
-- [("dbc","")]
-- > parse (string "dac") "dbc"
-- []

-- > parse (many digit) "123abc"
-- [("123","abc")]
-- > parse (many digit) "a123abc"
-- [("","a123abc")]
-- > parse (some digit) "a123abc"
-- []
-- > parse (some digit) "123abc"
-- [("123","abc")]


ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

-- > parse ident ("abc def")
-- [("abc"," def")]
-- > parse nat ("123 def")
-- [(123," def")]
-- > parse space ("     def")
-- [((),"def")]
-- > parse space ("     abc")
-- [((),"abc")]

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
        <|> nat

-- > parse int "-123 abc"
-- [(-123," abc")]
-- > parse int "123 abc"
-- [(123," abc")]
-- > parse int "abc"
-- []








