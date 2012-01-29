module Data.Character where
import Control.Arrow
import Control.Applicative hiding ( (<|>) )
import Data.Maybe
import Data.Utils
import Data.Note hiding ( names )
import qualified Data.Note as Note
import Data.List
import Control.Modifier hiding ( parse )
import qualified Control.Modifier as Mods
import Text.ParserCombinators.TagWiki
import Text.ParserCombinators.Parsec
import Data.Record hiding ( tags )

-- TODO: why is 'Anakara' under 'Rose'?
newtype Character = Character { base :: Note } deriving Eq

instance Record Character where
    note = base
    name = fromMaybe "" . maybeHead . reverse . names

instance Parseable Character where
    parser = do
        -- TODO: remove trails
        -- let mods = Mods.parse [category, qualifier, prefix, suffix]
        let mods = Mods.parse [category, qualifier, prefix, suffix, trail]
        (n, ms) <- parseNote mods
        let ps = prefixes ms
        let ss = suffixes ms
        pure Character{base = updateNote ps ss n}

updateNote :: [String] -> [String] -> Note -> Note
updateNote ps ss n = n{ Note.names = charNames ps ss (Note.names n)
                      , tags = charTags ps ss (map snd $ Note.names n) }

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
