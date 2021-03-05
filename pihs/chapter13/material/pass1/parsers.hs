import Control.Applicative
import Data.Char

-- type Parser = String -> [(a,String)] -- where the singleton list denotes success and empty list failure
-- in this definition, a parser can be viewed as a generalised form of state transformer

newtype Parser a = P (String -> [(a, String)]) -- to allow the Parse type to be made into instances of classes

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- > parse item ""
-- []
-- > parse item "abc"
-- [('a',"bc")]
