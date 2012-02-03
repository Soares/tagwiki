module Control.DateTime.Absolute
    ( Absolute(..)
    , plus
    , minus
    , clobber
    , diff
    , invert
    , expression
    , fromRel
    , toRel
    , normalize
    ) where
import Control.Applicative hiding ( (<|>), optional )
import Control.DateTime.Relative ( Relative(Relative) )
import Data.Maybe
import Data.String.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Printf
import Text.Regex
import qualified Control.DateTime.Relative as Relative
import qualified Text.Symbols as Y

data Absolute = Absolute
    { era    :: String
    , year   :: Int
    , month  :: Int
    , day    :: Int
    , hour   :: Int
    , prime  :: Int
    , second :: Int
    , extra  :: Int
    } | Present
    deriving Eq

-- NOTE: this is not perfect. Dates are hard. Only to be used once
-- before display.
normalize :: Absolute -> Absolute
normalize Present = Present
normalize (Absolute e y m d h p s x) = Absolute e y' m' d' h' p' s' x where
    (h', p', s', dd) = fixTime h p s
    (y', m', d') = fixDate y m (d + dd)

fixTime :: Int -> Int -> Int -> (Int, Int, Int, Int)
fixTime h p s = (h', p', s', rh) where
    kp a b = (mod a b, div a b)
    (s', rs) = kp s 100         -- 100 seconds to a prime
    (p', rp) = kp (p + rs) 100  -- 100 primes to an hour
    (h', rh) = kp (h + rp) 10   -- 10 hours to a day

fixDate :: Int -> Int -> Int -> (Int, Int, Int)
fixDate y 0 0 = (y, 0, 0)
fixDate y 0 1 | mod y 4 == 0 = (y, 0, 1)
fixDate y 0 n | mod y 4 == 0 = fixDate y 1 (n-1)
              | otherwise = fixDate y 1 n
fixDate y m 0 = fixDate y m 1
fixDate y m d | m `elem` [2, 5, 8, 11] && d <= 31 = fixMonth y m d
              | m `elem` [2, 5, 8, 11] && d >= 31 = fixDate y (m+1) (d-31)
              | d <= 30 = fixMonth y m d
              | d >= 30 = fixDate y (m+1) (d-31)
              | otherwise = fixMonth y m d

-- Assumes the day is correct if the month does not change
fixMonth :: Int -> Int -> Int -> (Int, Int, Int)
fixMonth y m d | m <= 12 = (y, m, d)
               | otherwise = fixDate (y+1) (m-12) d


toRel :: Absolute -> Relative
toRel Present = Relative.nowhen
toRel (Absolute _ y m d h p s x) = Relative
    (Just y) (Just m) (Just d)
    (Just h) (Just p) (Just s) (Just x)

fromRel :: String -> Relative -> Absolute
fromRel e (Relative y m d h p s x) = Absolute e
    (fromMaybe 0 y) (fromMaybe 0 m) (fromMaybe 0 d)
    (fromMaybe 0 h) (fromMaybe 0 p) (fromMaybe 0 s) (fromMaybe 0 x)

instance Show Absolute where
    show Present = "«present»"
    show a = case reads $ show $ toRel a of
        [] -> era a
        ((y, rest):_) -> reduce $ printf "%d%s%s" (y :: Int) (era a) rest where
            bad = mkRegex "(/0/0)?( 0:00)\\.0\\.0$"
            reduce x = strip $ subRegex bad x ""


plus, minus, clobber :: Absolute -> Relative -> Absolute
plus Present _ = Present
plus a b = fromRel (era a) $ Relative.plus (toRel a) b
minus Present _ = Present
minus a b = fromRel (era a) $ Relative.minus (toRel a) b
clobber Present _ = Present
clobber a b = fromRel (era a) $ Relative.clobber (toRel a) b

diff :: Absolute -> Absolute -> Maybe Relative
diff Present _ = Nothing
diff _ Present = Nothing
diff a b | era a /= era b = Nothing
         | otherwise = Just $ Relative.minus (toRel a) (toRel b)

invert :: String -> Absolute -> Absolute
invert _ Present = Present
invert e a = fromRel e $ Relative.invert $ toRel a


-- | Parse single absolute date
instance Parseable Absolute where
    parser = try (date <* whitespace >>= andTime)
         <|> try (yr <* whitespace >>= andTime)
         <|> try date
         <|> try yr
         <?> "an absolute moment" where
        yr = do
            y <- number
            whitespace
            e <- many1 letter
            pure (Absolute e y 0 0 0 0 0 0)
        date = do
            (Absolute e y _ _ _ _ _ _) <- yr
            operator "/"
            m <- number
            operator "/"
            d <- number
            pure (Absolute e y m d 0 0 0 0)
        andTime Present = return Present
        andTime (Absolute e y m d _ _ _ _) = do
            h <- number
            operator ":"
            p <- number
            s <- option 0 (operator "." *> number)
            x <- option 0 (operator "." *> number)
            return (Absolute e y m d h p s x)


-- | Parse a whole expression of absolute dates
expression :: GenParser Char st Absolute
expression = try (clobber <$> parser <*> (whitespace *> Relative.expression))
         <|> try (minus <$> parser <*> (sub *> Relative.expression))
         <|> try (plus <$> parser <*> (add *> Relative.expression))
         <|> parser
         <?> "absolute moment"

add, sub :: GenParser Char st ()
add = operator Y.addDate
sub = operator Y.subDate
