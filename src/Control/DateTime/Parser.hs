module Control.DateTime.Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import qualified Text.Symbols as Y
-- TODO: remove

-- Date expression operators
add, sub, oparen, cparen :: GenParser Char st ()
add = operator Y.addDate
sub = operator Y.subDate
oparen = operator Y.oParen
cparen = operator Y.cParen

-- Date expression operations
operations :: GenParser Char st Expression
operations = term `chainl1` addsub where
    addsub = try (add >> return Plus)
         <|> (sub >> return Minus)
         <?> "+/- date expression"
