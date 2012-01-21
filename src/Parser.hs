module Parser where
import Control.Applicative ( (<*) )
import Control.Monad
import Data.Either
import Data.Functor
import Record
import Text.ParserCombinators.Parsec

-- Top level
record :: FileType -> GenParser Char st Record
record ft = do
    record <- liftM2 (Record ft) firstLine secondLine
    fills <- fill `manyTill` eof
    return $ foldl (flip id) (record [] [] [] []) fills



-- First line
firstLine :: GenParser Char st [Name]
firstLine = (priTag `sepBy` comma) <* eol

-- A tag with priority attached
priTag :: GenParser Char st Name
priTag = whitespace >> liftM2 Name priority ptag

-- A plus marking priority
priority :: GenParser Char st Bool
priority = option False (plus >> return True)

-- An escaped plus
escPlus :: GenParser Char st String
escPlus = return <$> (hack >> char '+')

-- A tag that can escape a plus
ptag :: GenParser Char st String
ptag = liftM2 (++) (option "" escPlus) tag



-- Second line
secondLine :: GenParser Char st [Modifier]
secondLine = many modifier <* newline
modifier :: GenParser Char st Modifier
modifier = try (Cat <$> category)
       <|> try (Qal <$> qualifier)
       <|> try (Pre <$> prefix)
       <|> try (Suf <$> suffix)
       <|> try (Trl <$> trail)



-- Body
fill :: GenParser Char st (Record -> Record)
fill = try (addSec <$> section)
   <|> try (addAttr <$> attribute)
   <|> try (addEvent <$> event)
   <|> try (addApp <$> appearance) where
    addSec   x r = r{recordUnits=recordUnits r ++ x}
    addAttr  x r = r{recordAttrs=x:recordAttrs r}
    addEvent x r = r{recordEvents=x:recordEvents r}
    addApp   x r = r{recordAppearances=x:recordAppearances r}



-- Basic types
event :: GenParser Char st Event
event = bang >> liftM3 Event tag date block
appearance :: GenParser Char st Appearance
appearance = at >> liftM2 Appearance (reference <* optional at) block
attribute :: GenParser Char st Attribute
attribute = colin >> liftM3 Attribute key (many unit) block



-- Text chunks

-- The rest of the current line + all indented lines below
block :: GenParser Char st [Unit]
block = do
    clear <- many unit
    feed <- Str <$> eol
    white <- lookAhead whitespace
    if null white then return $ clear ++ [feed] else do
        blk <- blockWithWhite white
        return $ clear ++ (feed:blk)

-- Matches specific lines in a block
blockWithWhite :: String -> GenParser Char st [Unit]
blockWithWhite w = try (liftM2 (++) chunk $ blockWithWhite w) <|> return []
    where chunk = return . Str <$> many1 newline <|> lineWithWhite w

-- Block that doesn't care about whitespace
section :: GenParser Char st [Unit]
section = try (return . Str <$> many1 newline) <|> boringLine

-- Lines with just the right amount of white
lineWithWhite :: String -> GenParser Char st [Unit]
lineWithWhite "" = boringLine
lineWithWhite (w:ws) = (char w >> lineWithWhite ws) <?> "moar whitespace!"

-- Lines that aren't keys, events, or appearances
boringLine :: GenParser Char st [Unit]
boringLine = (do
    notFollowedBy bang
    notFollowedBy colin
    notFollowedBy at
    many1 unit
    ) <?> "boring old line"



-- Units
-- One link, date, or string of markdown text
unit :: GenParser Char st Unit
unit = try (Lnk <$> link)
   <|> try (Dxp <$> calculation)
   <|> try (Str <$> text)
   <?> "link, date, text, or line eol"



-- Links
link :: GenParser Char st (Reference, Maybe String)
link = try (between pipe pipe $ refAnd $ return Nothing)
   <|> try (between pipe pipe $ refAnd $ Just <$> (comma >> linkText))
   <?> "link" where refAnd = liftM2 (,) reference
linkText :: GenParser Char st String
linkText = unit `manyTill` "|"



-- References
catOrQual :: GenParser Char st (Either Category Qualifier)
catOrQual = try (Left <$> category)
        <|> try (Right <$> qualifier)
        <?> "category or qualifier"
reference :: GenParser Char st Reference
reference = do
    tg <- tag
    (cats, quals) <- partitionEithers <$> many catOrQual
    evs <- many (bang >> tag)
    return $ Reference tg cats quals evs



-- Dates
-- A date for an event (either absolute or calculated)
eventDate :: GenParser Char st Calculation
eventDate = optional at >> whitespace >> date where
    date = try nakedDate
       <|> try calculation
       <?> "a date for an event"

-- Date calculations
calculation :: GenParser Char st Calculation
calculation = try (between obrace cbrace (lift Exactly expression))
       <|> try (between obrace cbrace (liftM2 Range expression endExpr))
       where endExpr = comma >> whitespace >> option Present expression

-- Date expressions
expression, operations, term :: GenParser Char st Expression
expression = operations `chainl1` (whitespace >> return Clobber)
operations = term `chainl1` addsub where
    addsub = try (plus >> return Plus)
         <|> try (minus >> return Minus)
         <?> "add / sub expression"
term = try (between oparen cparen expression)
   <|> try (More <$> (plus >> expression))
   <|> try (Less <$> (minus >> expression))
   <|> try (questionMark >> return Unknown)
   <|> try (Abs <$> (free absDate))
   <|> try (Rel <$> (free relDate))
   <|> try (At  <$> (free time))
   <|> try (Rel <$> (free year))
   <|> try (From <$> reference <* at)
   <|> try (From <$> reference <* anyWhite)
   <?> "simple expression"

-- Naked dates (must be absolute, may include time)
nakedDate :: GenParser Char st Calculation
nakedDate = Exactly <$> (try getDateAndTime <|> try getDate <?> "date") where
    getDateAndTime = liftM2 Clobber getDate (At <$> time)
    getDate = Abs <$> absDate

-- Absolute Date
absDate :: GenParser Char st AbsDate
absDate = getYear >>= getMonth >>= getDay where
    make yr  = AbsDate yr Nothing Nothing
    getYear     = make                     <$> liftM2 Year number era
    getMonth dt = (\m -> dt{absMonth=m}) <$> sec slash
    getDay   dt = (\d -> dt{absDay=d  }) <$> sec slash

-- Relative Date
relDate :: GenParser Char st RelDate
relDate = getYear >>= getMonth >>= getDay where
    getYear     = (\y -> whenever{relYear=y     }) <$> maybeNumber <* slash
    getMonth dt = (\m -> dt    {relMonth=m    }) <$> maybeNumber
    getDay   dt = (\d -> dt    {relDay=d      }) <$> sec slash

-- Time
time :: GenParser Char st Time
time = getHour >>= getMinute >>= getSecond >>= getDetail where
    getHour     = (\h -> noclock{hour=h}) <$> maybeNumber <* colin
    getMinute t = (\m -> t{minute=m }) <$> maybeNumber
    getSecond t = (\s -> t{second=s }) <$> sec dot
    getDetail t = (\d -> t{detail=d }) <$> sec dot

-- A lonely lonely year
year :: GenParser Char st RelDate
year = (\y -> whenever{relYear=Just y}) <$> number



-- Strings that mean things
xTag, xText, xKey, xModifier :: String
xTag = "@,#(){}!|+-/\n"
xKey = " \t\n"
xText = "|{\n"
xModifier = '^':'$':xTag

tag, key, text, modText :: GenParser Char st String
tag = except xTag
key = except xKey
text = many1 $ try escWhite <|> escaping xText
modText = except xModifier

category, qualifier, prefix, suffix, trail :: GenParser Char st String
category = hash >> modText
qualifier = between oparen cparen modText
prefix = carat >> modText
suffix = dollar >> modText
trail = comma >> modText



-- Operators

-- Operators that occur at the top level
bang, at :: GenParser Char st ()
bang = designator '!'
at = designator '@'
-- ':' appears both here and in date/time

-- Operators that don't occur in normal text
oparen, cparen, hash, comma, carat, dollar :: GenParser Char st ()
hash = designator '#'
comma = designator ','
carat = designator '^'
dollar = designator '$'
oparen = designator '('
cparen = designator ')'

-- Operators used for date and time
slash, colin, dot :: GenParser Char st ()
slash = operator '/'
colin = operator ':'
dot = operator '.'

-- Operators that only occur in expressions
plus, minus, questionMark :: GenParser Char st ()
plus = operator '+'
minus = operator '-'
questionMark = operator '?'

-- Operators that occur in normal text
pipe, obrace, cbrace :: GenParser Char st ()
pipe = char '|' >> return ()
obrace = char '{' >> anyWhite >> return ()
cbrace = anyWhite >> char '}' >> return ()



-- Number handling

-- Parses zero or more digits, returning an Int if there were any digits
maybeNumber :: GenParser Char st (Maybe Int)
maybeNumber = many digit >>= \nums -> return $ case nums of
    "" -> Nothing
    x -> Just $ read x

-- Parses and reads one or more digits
number :: GenParser Char st Int
number = read <$> many1 digit



-- Special strings
quadrupleHack, whitespace, anyWhite, era, eol :: GenParser Char st String
quadrupleHack = hack >> char 's' >> return "\\\\\\\\"
whitespace = many $ oneOf " \t"
anyWhite = many $ oneOf " \t\n"
era = many1 letter
eol = try (return <$> newline) <|> (eof >> return "")

-- Special Characters
hackhack, escWhite :: GenParser Char st Char
hackhack = hack >> hack >> return '\\'
scWhite = try (hack >> space) <|> try (hack >> tab) <?> "escaped whitespace"

-- Special Actions
hack, quote :: GenParser Char st ()
hack = char '\\' >> return ()
quote = char '"' >> return ()



-- Helper parsers

-- Parses a character not present in `chars`, unless escaped with a backslash.
-- If escaped, 'unescapes' the character in the result.
escaping :: String -> GenParser Char st Char
escaping chars = try hackhack
             <|> try (hack >> oneOf chars)
             <|> noneOf chars
             <?> "[^" ++ chars ++ "] or an escape sequence"

-- Parse a whole string of escaping characters
except :: String -> GenParser Char st String
except = many1 . escaping

-- Either an operator, an operator and a number, or nothing at all.
-- Reduces to a number
sec :: GenParser Char st () -> GenParser Char st (Maybe Int)
sec op = try (op >> maybeNumber) <|> return Nothing

-- A whitespace-wrapped parser
free :: GenParser Char st a -> GenParser Char st a
free p = anyWhite >> p <* anyWhite

-- A whitespace-wrapped no-op action
operator :: Char -> GenParser Char st ()
operator c = anyWhite >> char c >> anyWhite >> return ()

-- A whitespace-wrapped one-line no-op action
designator :: Char -> GenParser Char st ()
designator c = whitespace >> char c >> return ()
