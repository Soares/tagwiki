module Control.DateTime.Sink where
import Control.Applicative hiding ( many, (<|>) )
import Control.Dangerous hiding ( Warning )
import Internal
import Text.Printf

data Unknown = Unknown deriving Eq
data Present = Present deriving Eq

instance Show Unknown where show = const "«unknown»"
instance Show Present where show = const "«present»"

instance Moment Unknown where
    year = const Nothing
    month = const Nothing
    day = const Nothing
    hour = const Nothing
    prime = const Nothing
    second = const Nothing
    extra = const Nothing
    era = const Nothing
    plus x _ = warn (InvalidOperation "add to" $ show x) *> pure x
    minus x _ = warn (InvalidOperation "subtract from" $ show x) *> pure x
    clobber x _ = warn (InvalidOperation "clobber" $ show x) *> pure x
instance Moment Present where
    year = const Nothing
    month = const Nothing
    day = const Nothing
    hour = const Nothing
    prime = const Nothing
    second = const Nothing
    extra = const Nothing
    era = const Nothing
    plus x _ = warn (InvalidOperation "add to" $ show x) *> pure x
    minus x _ = warn (InvalidOperation "subtract from" $ show x) *> pure x
    clobber x _ = warn (InvalidOperation "clobber" $ show x) *> pure x

data Warning = InvalidOperation String String
instance Show Warning where
    show (InvalidOperation op x) = printf "Can't %s %s" op x

{- TODO: Remove
module Control.DateTime.Moment
    ( Moment(..)
    , Momentus(..)
    , Direction(..)
    , Offset(..)
    , offset
    , present
    , plus
    , minus
    , negate
    , clobber
    ) where
import Control.Applicative hiding ( (<|>) )
import Control.Dangerous
import Control.DateTime.Parser
import Control.Monad hiding ( guard )
import Data.Maybe
import Data.Utils
import Prelude hiding ( negate )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Internal ( Internal(..) )
    
-- TODO: Add timezones

data Moment = Moment
    { values :: [Maybe Int]
    , era    :: String
    } deriving Eq

data Direction
    = Before
    | After
    deriving (Eq, Ord, Show)

data Offset
    = Descending Direction Moment
    | Root
    deriving (Eq, Show)

offset :: Offset -> Moment
offset (Descending _ mo) = mo
offset Root = present

present :: Moment
present = Moment{values=[], era=""}

plus :: (Internal m) => Moment -> Moment -> m Moment
plus (Moment xs ex) (Moment ys ey) | null ex || null ey = pure simple
                                   | otherwise = complex where
    simple = Moment (add xs ys) (if null ey then ex else ey)
    complex = flip Moment ey <$> zs
    zs = add ys <$> rebase xs ex ey

minus :: (Internal m) => Moment -> Moment -> m Moment
minus x y = plus x (negate y)

negate :: Moment -> Moment
negate (Moment xs e) = Moment (neg xs) e

clobber :: Moment -> Moment -> Moment
clobber (Moment xs ex) (Moment ys ey) = Moment zs ez where
    right = flip maybe Just
    zs = zipAll right xs ys
    ez = if null ey then ex else ey

add :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
add = zipAll $ liftM2 (+)
sub :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
sub = zipAll $ liftM2 (-)
neg :: [Maybe Int] -> [Maybe Int]
neg = map $ fmap (0-)

rebase :: (Internal m) => [Maybe Int] -> String -> String -> m [Maybe Int]
rebase xs ex ey = check *> ys where
    check = canRelateEras ex ey >>= (`unless` err)
    err = throw $ NotRelateable ex ey
    ys = sub <$> normal xbase <*> normal ybase
    xbase = Moment xs ex
    ybase = Moment [] ey

normal :: (Internal m) => Moment -> m [Maybe Int]
normal (Moment xs "") = pure xs
normal (Moment xs e) = maybe (pure xs) recurse =<< lookupEraCode e where
    recurse (Descending After mo) = add xs <$> normal mo
    recurse (Descending Before mo) = add (neg xs) <$> normal mo
    recurse Root = normal (Moment xs "")

instance Parseable Moment where
    parser = try (date >>= andTime)
         <|> try date
         <|> try time
         <|> lonelyYear
         <?> "a moment in time" where
        date = try absolute <|> relative
        andTime d = clobber d <$> (whitespace *> time)
        absolute = number >>= andEra >>= andMonth >>= andDay where
            andEra y = Moment [Just y] <$> many1 letter
            andMonth  d = set 1 d <$> markedInt slash
            andDay    d = set 2 d <$> markedInt slash
        relative = year >>= andMonth >>= andDay where
            year = set 0 (Moment [] "") <$> maybeInt <* slash
            andMonth  d = set 1 d <$> maybeInt
            andDay    d = set 2 d <$> markedInt slash
        time = hour >>= andMinute >>= andSecond >>= andDetail where
            hour = set 3 (Moment [] "") <$> maybeInt <* colin
            andMinute d = set 4 d <$> maybeInt
            andSecond d = set 5 d <$> markedInt dot
            andDetail d = set 6 d <$> markedInt dot
        lonelyYear = set 0 (Moment [] "") . Just <$> number
        set :: Int -> Moment -> Maybe Int -> Moment
        set i (Moment xs e) n = Moment (update xs i n) e
        update [] 0 n = [n]
        update [] i n = update [Nothing] (i-1) n
        update (_:xs) 0 n = n:xs
        update (x:xs) i n = x:update xs (i-1) n

instance Momentus Moment where
    moment = pure

instance Show Moment where
    show (Moment xs e) = y ++ e ++ m ++ d ++ h ++ n ++ s ++ l where
        y = at 0
        m = shw "/" 1
        d = shw "/" 2
        h = shw " " 3
        n = shw ":" 4
        s = shw "." 5
        l = shw "." 6
        shw str i = if null (at i) then "" else str ++ at i
        at i | i < length xs = maybe "" show (xs !! i)
             | otherwise = ""

class Momentus a where moment :: (Internal m) => a -> m Moment

data Error = NotRelateable String String deriving Show
-}
