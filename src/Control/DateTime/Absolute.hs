module Control.DateTime.Absolute
    ( Absolute(..)
    , Modification(Done)
    , apply
    , plus
    , minus
    , clobber
    , diff
    , invert
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

-- NOTE: this is not transitive. Use only once before display.
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
--  | This is not easy because zero days are not allowed except on the
--  | zero month. So this transform is NOT transitive nor associative.
--  | Only use it once before display, as it is a bit lossy.
-- Tests:
--   fixDate 201 0 (-1) == (200,12,30)
--   fixDate 201 3 (-1) == (200,2,31)
--   fixDate 307 1 (-1) == (307,0,0)
--   fixDate 400 1 (-1) == (400,0,1)
--   fixDate 307 0  1   == (307,1,1)
--   fixDate 307 0  2   == (307,1,2)
--   fixDate 200 0  1   == (200,0,1)
--   fixDate 200 0  2   == (307,1,1)
--   fixDate 200 1  31  == (200,2,1)
--   fixDate 200 2  35  == (200,3,4)
fixDate y m d
--  | Day less than zero
    | d < 0 = fixDate y (m - 1) (d + daysIn y (m-1) + 1)
--  | Everything looks just peachy (day-wize.) Call the mo/yr fixer.
    | d > 0 && d <= daysIn y m = fixMonth y m d
--  | Day too large:
--  | This gets a bit fishy because of how zero-days are recorded.
--  | (Stupid naturalistic date system.)
    | d > daysIn y m = fixMonth y (m + 1) (d - daysIn y m)
--  | This is where the non-transitivity comes in.
--  | 0-day is displayed as 1-day on non-zero months. :C
    | d == 0 && isZeroMonth m = fixMonth y m d
    | d == 0 = fixMonth y m 1
--  | I don't think we missed any cases, but the compiler won't trust me.
--  | This is what we do when something odd comes up: pass it off!
    | otherwise = fixMonth y m d

daysIn :: Int -> Int -> Int
daysIn y m | isZeroMonth m && isLeapYear y = 1
           | isZeroMonth m = 0
           | mod m 13 `elem` [2, 5, 8, 11] = 31
           | otherwise = 30

isLeapYear :: Int -> Bool
isLeapYear y = mod y 4 == 0

isZeroMonth :: Int -> Bool
isZeroMonth m = mod m 13 == 0

-- Assumes the day is correct if the month does not change
fixMonth :: Int -> Int -> Int -> (Int, Int, Int)
--  | 200/-1/1 → 199/12/1
fixMonth y m d | m < 0 = fixDate (y-1) (m+13) d
--  | All m == 0 cases should have been corrected in fixDate
fixMonth y 0 d = (y, 0, d)
--  | Now we may assume m > 0
fixMonth y m d
--  | Everything looks fine
    | m <= 12 = (y, m, d)
--  | m is out of range.
--  | 200/13/2 → 201/0/2 (→ 201/1/1)
    | otherwise = fixDate (y+1) (m-13) d


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
    show a = case reads $ show $ toRel $ normalize a of
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
            _ <- whitespace
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



data Modification
    = Operation (Absolute -> Relative -> Absolute) Relative Modification
    | Done

instance Show Modification where
    show (Operation _ rel m) = "Op (" ++ show rel ++ ") (" ++ show m ++ ")"
    show Done = "Done"

apply :: Absolute -> Modification -> Absolute
apply a (Operation fn rel m) = apply (fn a rel) m
apply a Done = a

instance Parseable Modification where
    parser = try (Operation clobber <$> (clob *> parser) <*> floating parser)
        <|> try (Operation plus <$> (add *> parser) <*> floating parser)
        <|> try (Operation minus <$> (sub *> parser) <*> floating parser)
        <|> pure Done

add, sub, clob :: GenParser Char st ()
clob = operator Y.clobDate
add = operator Y.addDate
sub = operator Y.subDate
