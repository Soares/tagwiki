module Text.ParserCombinators.TagWiki where
import Control.Applicative hiding ( many, (<|>) )
import Data.List
import Text.ParserCombinators.Parsec

class Parseable a where
    parser :: GenParser Char st a

-- Parses and reads one or more digits
number :: GenParser Char st Int
number = read <$> many1 digit

real :: (RealFrac r, Read r) => GenParser Char st r
real = read <$> str where
    str = (++) <$> int <*> option "" decimal
    decimal = char '.' *> int
    int = many1 digit

-- \s → \\\\
quadrupleHack :: GenParser Char st String
quadrupleHack = hack *> char 's' *> return "\\\\\\\\"

-- Excludes newlines
whitespace :: GenParser Char st String
whitespace = many $ oneOf " \t"

-- Includes newlines
anyWhite :: GenParser Char st String
anyWhite = many $ oneOf " \t\n"

-- newline or EOF
eol :: GenParser Char st String
eol = try (return <$> newline) <|> (eof *> return "")

-- \\ -> \
hackhack :: GenParser Char st Char
hackhack = hack *> hack *> return '\\'

-- '\ ' -> space, \t -> tab, '\n' -> newline
escWhite :: GenParser Char st Char
escWhite = try (hack *> space)
       <|> try (hack *> tab)
       <|> try (hack *> newline)
       <?> "escaped whitespace"

-- Ignored hack
hack :: GenParser Char st ()
hack = char '\\' *> return ()

-- Parses a character not present in `chars`, unless escaped with a backslash.
-- If escaped, 'unescapes' the character in the parsed string.
-- Ex.
--      parseTest (escaping "abc") "\a \b \c" → "a b c"
--      parseTest (escaping "abc") "a  \b \c" → error (unescaped a)
escaping :: String -> GenParser Char st Char
escaping chars = try hackhack
             <|> try (hack *> oneOf chars)
             <|> noneOf (nub chars)
             <?> "[^" ++ oneLine chars ++ "] or an escape sequence"
             where oneLine = intercalate "\\n" . lines

-- Parse a whole string of escaping characters
except :: String -> GenParser Char st String
except = many1 . escaping

-- A whitespace-wrapped parser
floating :: GenParser Char st a -> GenParser Char st a
floating p = anyWhite *> p <* anyWhite

-- An multiline operator
operator :: String -> GenParser Char st ()
operator c = anyWhite *> string c *> anyWhite *> return ()

-- A symbol with optional whitespace preciding it
designator :: String -> GenParser Char st ()
designator c = whitespace *> string c *> return ()

-- A symbol with optional whitespace before and after (no newlines)
marker :: String -> GenParser Char st ()
marker c = whitespace *> string c *> whitespace *> return ()
