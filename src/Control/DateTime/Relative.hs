module Control.DateTime.Relative
    ( Relative(..)
    , nowhen
    , plus
    , minus
    , invert
    , clobber
    , expression
    ) where
import Control.Applicative hiding ( (<|>) )
import Data.String.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.Regex
import qualified Text.Symbols as Y

-- | Definition
data Relative = Relative
    { year   :: Maybe Int
    , month  :: Maybe Int
    , day    :: Maybe Int
    , hour   :: Maybe Int
    , prime  :: Maybe Int
    , second :: Maybe Int
    , extra  :: Maybe Int
    } deriving (Eq, Ord)



-- | Basic relative moments
nowhen :: Relative
nowhen = Relative Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- | How to print
instance Show Relative where
    show (Relative y m d h p s x) = reduce $ printf "%s%s%s%s%s%s%s"
        (shw "" y) (shw "/" m) (shw "/" d)
        (shw " " h) (shw2 ":" p) (shw "." s) (shw "." x) where
        shw str = maybe str (printf "%s%d" str)
        shw2 str = maybe str (printf "%s%02d" str)
        bad = mkRegex ":\\.\\."
        reduce a = strip $ subRegex bad a ""


-- Operations you can do on relative dates
plus, minus, clobber :: Relative -> Relative -> Relative
plus = op $ maybify (+)
minus = op $ maybify (-)
clobber = op right

invert :: Relative -> Relative
invert (Relative y m d h p s x) = Relative
    (neg y) (neg m) (neg d) (neg h) (neg p) (neg s) (neg x)
    where neg = fmap negate


-- | Utilities & Helpers for doing calculations on relative dates
op :: (Maybe Int -> Maybe Int -> Maybe Int) -> Relative -> Relative -> Relative
op fn a b = Relative y m d h p s x where
    y = fn (year a) (year b)
    m = fn (month a) (month b)
    d = fn (day a) (day b)
    h = fn (hour a) (hour b)
    p = fn (prime a) (prime b)
    s = fn (second a) (second b)
    x = fn (extra a) (extra b)

maybify :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
maybify _ Nothing x = x
maybify _ x Nothing = x
maybify fn (Just x) (Just y) = Just $ fn x y

-- Select the value on the right
right :: Maybe a -> Maybe a -> Maybe a
right x Nothing = x
right _ y = y


-- | How to parse
instance Parseable Relative where
    parser = try (fromList <$> (date >>= andTime))
         <|> try (fromList <$> date)
         <|> try (fromList <$> andTime [Nothing, Nothing, Nothing])
         <|> lonelyYear
         <?> "a relative moment" where

        -- Chunks of relative
        date = relative $ strings "//"
        time = try (relative $ strings ":..")
           <|> try (relative $ strings ":.")
           <|> relative (strings ":")
           <?> "a relative time"
        lonelyYear = fromList . pure . Just <$> number
        andTime ymd = (ymd ++) <$> time

        -- Utilities
        strings = map pure

        fromList :: [Maybe Int] -> Relative
        fromList xs = Relative
            (get 0 xs) (get 1 xs) (get 2 xs)
            (get 3 xs) (get 4 xs) (get 5 xs) (get 6 xs)

        get :: Int -> [Maybe Int] -> Maybe Int
        get _ [] = Nothing
        get 0 (x:_) = x
        get n (_:xs) = get (n-1) xs

        -- Parse a bunch of optional markers
        relative :: [String] -> GenParser Char st [Maybe Int]
        relative markers = try (full markers)
                       <|> ((Nothing:) <$> headless markers)

        -- Parse with a head (i.e. 10/3/2)
        full :: [String] -> GenParser Char st [Maybe Int]
        full [] = pure <$> optionMaybe number
        full (m:ms) = do
            h <- number
            operator m
            t <- full ms
            return (Just h:t)

        -- Parse without a head (i.e. /3/2)
        headless :: [String] -> GenParser Char st [Maybe Int]
        headless [] = pure []
        headless (m:ms) = do
            operator m
            h <- optionMaybe number
            t <- headless ms
            return (h:t)

-- | Parse a whole expression of Relatives
expression :: GenParser Char st Relative
expression = operations `chainl1` (whitespace *> pure clobber)

operations :: GenParser Char st Relative
operations = term `chainl1` addsub where
    addsub = try (add *> pure plus)
         <|> (sub *> pure minus)
         <?> "+/- date expression"

-- 'simple' terms
term :: GenParser Char st Relative
term = try (between oparen cparen expression)
   <|> floating parser
   <?> "simple relative moment"


-- Operators that only occur in expressions
add, sub, oparen, cparen :: GenParser Char st ()
add = operator Y.addDate
sub = operator Y.subDate
oparen = operator Y.oParen
cparen = operator Y.cParen
