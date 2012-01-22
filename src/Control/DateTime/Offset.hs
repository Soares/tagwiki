module Control.DateTime.Offset 
    ( Offset
    , era
    , diff
    , known
    , root
    ) where
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad
import Data.String.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import qualified Text.Symbols as Y

data Offset = Offset [Int] String
            | Fuzzy [Int] [Int] String
            deriving (Eq, Ord)

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
