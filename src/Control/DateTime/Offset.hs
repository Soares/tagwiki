module Control.DateTime.Moment
    ( Moment
    , Momentus
    , plus
    , minus
    , negate
    ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad
import Data.String.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import qualified Text.Symbols as Y

data Moment = Relative [Maybe Int]
            | Absolute [Maybe Int] String

date :: Moment -> [Maybe Int]
date (Relative xs) = xs
date (Absolute xs _) = xs

era :: Moment -> Maybe String
era (Relative _) = Nothing
era (Absolute _ e) = e

isAbsolute, isRelative :: Moment -> Bool
isAbsolute (Absolute _ _) = True
isAbsolute _ = False
isRelative (Relative _) = True
isRelative _ = False

plus :: Moment -> Moment -> RestrictedOperation Moment
plus (Absolute xs ex) (Absolute ys ey) = flip Absolute ey <$> rebase xs ex ey
plus (Relative xs) (Relative ys) = pure $ Relative $ add xs ys
plus (Relative xs) (Absolute ys e) = pure $ Absolute (add xs ys) e
plus a r = plus r a

minus :: Moment -> Moment -> Moment
minus x y = plus x (negate y)

negate :: Moment -> Moment
negate (Relative xs) = Relative (neg xs)
negate (Absolute xs e) = Absolute (neg xs) e

add :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
add = zipAll $ liftM2 (+)
sub :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
sub = zipAll $ liftM2 (-)
neg :: [Maybe Int] -> [Maybe Int]
neg = map $ fmap (0-)

root :: String -> RestrictedOperation (Maybe String)
root (Relative _) = pure Nothing
root (Absolute _ e) = select <$> dawn e where
    select = maybe (pure Nothing) (root . snd)

relateable :: String -> String -> RestrictedOperation Bool
relateable x y = maybe True (uncurry (==)) <$> tuple
    where tuple = (,) <$> root x <*> root y

-- TODO: warn instead
convert :: Moment -> String -> RestrictedOperation Moment
convert (Relative xs) e = pure $ Absolute xs e
convert a e = check *> make where
    make = flip Absolute e <$> (sub <*> normal x <*> normal y)
    check = unless <$> relateable x y <*> warning
    warning = lift . lift . throw $ NotRelateable x y

rebase :: [Maybe Int] -> String -> String -> RestrictedOperation [Maybe Int]
rebase xs ex ey = check *> ys where
    ys = sub <$> normal xbase <*> normal ybase
    check = unless <$> relateable ex ey <*> die
    die = lift . lift . throw $ NotRelateable x y
    xbase = Absolute xs ex
    ybase = Absolute [] ey

normal :: Moment -> RestrictedOperation [Maybe Int]
normal (Relative xs) = pure xs
normal (Absolute xs e) = maybe (pure xs) recurse (dawn e) where
    recurse (dir, m) = add (direct dir xs) <$> normal m

direct :: Direction -> [Maybe Int] -> [Maybe Int]
direct Positive = id
direct Negative = neg







era :: Offset -> String
era (Offset _ e) = e
era (Fuzzy _ _ e) = e

diff :: Offset -> [Int]
diff (Offset xs _) = xs
diff _ = error "Can't find diff for fuzzy offset"

known :: [Offset] -> [Offset]
known [] = []
known (Fuzzy _ _ _:_) = []
known (x:xs) = x:known xs

root :: [Offset] -> String
root [] = []
root (x:[]) = era x
root (_:xs) = root xs


-- Printing
instance Show Offset where
    show (Offset xs e) = display 0 xs ++ e
    show (Fuzzy xs ys e) = display 0 xs ++ e ++ " ~ " ++ display 0 ys

display :: Int -> [Int] -> String
display _ [] = ""
display n (x:xs) | n == 0 = rest
                 | n == 1 || n == 2 = '/':rest
                 | n == 3 = ' ':rest
                 | n == 4 = ':':rest
                 | otherwise = '.':rest
                 where rest = show x ++ display (n+1) xs



-- Parsing
instance Parseable Offset where
    parser = try range
         <|> Offset <$> date <*> eraStr
         <?> "era offset"

range :: GenParser Char st Offset
range = liftM3 Fuzzy date (sep >> date) eraStr

date :: GenParser Char st [Int]
date = cons (whitespace >> number) month where
    month = cons (slash >> number) day
    day = cons (slash >> number) hour
    hour = cons (whitespace >> number) minute
    minute = cons (colin >> number) second
    second = cons (dot >> number) detail
    detail = cons (dot >> number) (return [])
    cons one rest = try (liftM2 (:) one rest) <|> return []

eraStr :: GenParser Char st String
eraStr = strip <$> (whitespace >> except restricted)

slash, colin, dot, sep :: GenParser Char st ()
slash = marker Y.dateSep
colin = marker Y.minSep
dot = operator Y.secSep
sep = marker Y.offsetSep

restricted :: String
restricted = concat [Y.category, Y.prefix, Y.suffix, Y.trail
                    , Y.oQualifier, Y.cQualifier, "\n"]
