module Control.DateTime.Relative
    ( Relative(..)
    , nowhen
    , plus
    , minus
    , invert
    , clobber
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
        bad = mkRegex ":?\\.\\."
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
    parser = try full
         <|> try date
         <|> try time
         <|> lonelyYear
         <?> "a relative moment" where

        full = plus <$> date <*> (whitespace *> time)
        date = headDate <|> headlessDate <?> "relative date"
        time = headTime <|> headlessTime <?> "relative time"

        headDate = do
            y <- number <* slash
            m <- optionMaybe number
            d <- slash *> optionMaybe number
            pure $ Relative (Just y) m d Nothing Nothing Nothing Nothing
        headlessDate = do
            m <- slash *> optionMaybe number
            d <- slash *> optionMaybe number
            pure $ Relative Nothing m d Nothing Nothing Nothing Nothing

        headTime = do
            h <- number <* colin
            p <- optionMaybe number
            s <- optionMaybe $ dot *> number
            x <- optionMaybe $ dot *> number
            pure $ Relative Nothing Nothing Nothing (Just h) p s x

        headlessTime = do
            p <- colin *> optionMaybe number
            s <- optionMaybe $ dot *> number
            x <- optionMaybe $ dot *> number
            pure $ Relative Nothing Nothing Nothing Nothing p s x

        lonelyYear = do
            y <- number
            pure $ Relative (Just y) Nothing Nothing
                Nothing Nothing Nothing Nothing

slash, colin, dot :: GenParser Char st ()
slash = string Y.dateSep *> return ()
colin = string Y.minSep *> return ()
dot = string Y.secSep *> return ()
