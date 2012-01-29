module Data.Character where
import Control.Applicative hiding ( (<|>) )
import Control.Arrow
import Control.Modifier hiding ( parse )
import Data.List
import Data.Maybe
import Data.Note hiding ( names )
import Data.Record hiding ( tags )
import Data.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Utils
import qualified Control.Modifier as Mods
import qualified Data.Note as Note

-- TODO: why is 'Anakara' under 'Rose'?
-- TODO: why are full names in qualifiers?
newtype Character = Character { base :: Note } deriving (Eq, Ord)

instance Record Character where
    note = base
    name = fromMaybe "" . maybeHead . reverse . names

instance Parseable Character where
    parser = do
        (n, ms) <- parseNote Mods.anyMod
        let ps = prefixes ms
        let ss = suffixes ms
        pure Character{base = updateNote ps ss n}

updateNote :: [String] -> [String] -> Note -> Note
updateNote ps ss n = n{ Note.names = charNames ps ss (Note.names n)
                      , tags = charTags ps ss (map snd $ Note.names n)
                      , Note.qualifiers = charQuals (map snd $ Note.names n)
                                                    (Note.qualifiers n) }

-- The names used internatlly to match pins
-- Will be turned into References automatically,
-- so categories and qualifiers will be taken care of.
-- Prefixes and suffixes will not.
-- The Bool denotes priority, and should be threaded through
-- to all names that inherit priority.
charNames :: [String] -> [String] -> [(Bool, String)] -> [(Bool, String)]
charNames _ _ [] = []
charNames ps ss ((priority, primaryName):ns) = primary ++ secondary where
    primary = [(priority, x) | x <- expand primaryName]
    secondary = map (second standardName) ns
    expand = nub . addSuffixes ss . applyPrefixes ps . splitIntoNames

-- Nicknames can be qualifiers
charQuals :: [String] -> [String] -> [String]
charQuals ns qs = nub $ map normalize ns ++ qs

standardName :: String -> String
standardName = unwords . splitIntoNames

addSuffixes :: [String] -> [String] -> [String]
addSuffixes [] xs = xs
addSuffixes ss xs = xs ++ [unwords (x:ss) | x <- xs]

-- Assumes that informal name comes first
applyPrefixes :: [String] -> [String] -> [String]
applyPrefixes _ [] = []
applyPrefixes ps [x] = x : [unwords [p, x] | p <- ps]
applyPrefixes ps (x:ys) = x : ys ++ [unwords [p, y] | p <- ps, y <- ys]

splitIntoNames :: String -> [String]
splitIntoNames str = case parseNames str of
    [] -> [str]
    [x] -> [x]
    [x, y] -> [x, y, unwords [x, y]]
    xs -> [x, z, unwords [x, z], unwords xs] where
        x = head xs
        z = last xs

parseNames :: String -> [String]
parseNames str = case parse namesParser str str of
    Left _ -> [str]
    Right ns -> ns

namesParser :: GenParser Char st [String]
namesParser = many1 (whitespace *> nameParser <* whitespace)

nameParser :: GenParser Char st String
nameParser = except " \t\""


-- The tags to use for vim.
-- There should be only one per pseudonym, they can be found
-- fuzzily using editor tools
charTags :: [String] -> [String] -> [String] -> [String]
charTags ps ss = map expand where
    expand n = unwords (ps ++ [n] ++ ss)
