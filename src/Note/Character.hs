module Note.Character where
import Control.Applicative hiding ( (<|>) )
import Control.Modifier ( prefixes, suffixes )
import Data.List
import Data.Maybe
import Data.Record hiding ( tags )
import Data.String.Utils
import Data.Utils
import Note hiding ( names )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pinpoint ( Pinpoint, fromName )
import qualified Control.Modifier as Mods
import qualified Data.Note as Note

newtype Character = Character { base :: Note } deriving (Eq, Ord)

instance Record Character where
    note = base
    name = fromMaybe "" . maybeHead . reverse . names

makeCharacter :: FilePath -> GenParser Char st Character
makeCharacter fp = do
    (n, ms) <- parseNote fp Mods.anyMod
    let ps = prefixes ms
    let ss = suffixes ms
    pure Character{base = updateNote ps ss n}

updateNote :: [String] -> [String] -> Note -> Note
updateNote ps ss n = n{ Note.names = charNames ps ss (Note.names n)
                      , tags = charTags ps ss (map snd $ Note.names n)
                      , Note.qualifiers = Note.qualifiers n ++ qs } where
    qs = charQualifiers (drop 1 $ map snd $ Note.names n)


-- | Adds prefixes and suffixes to tags.
-- | Character names will be split on spaces, that they may be referenced
-- | by either first or last or full names.
-- |
-- | If the character has multi-part names (i.e. "Van Halen"), escape the
-- | whitespace (i.e. "Van\ Halen").
-- |
-- | Only the first name is so split; all following names will not be touched.
charNames :: [String] -> [String] -> [(Bool, String)] -> [(Bool, String)]
charNames _ _ [] = []
charNames ps ss ((pri, n):ns) = nub $ primary ++ ns where
    expand = addSuffixes ss . applyPrefixes ps . splitIntoNames
    primary = [(pri, x) | x <- expand n]


-- | Updates a character to add 'nicknames' to the qualifiers.
-- | Thus, if you have the following character:
-- |
-- |    Fredward Sharpe, Freddie
-- |
-- | He may be referenced as (for example):
-- |
-- |    |Fredward (Freddie) Sharpe|
charQualifiers :: [String] -> [Pinpoint]
charQualifiers = nub . map fromName

-- | Add all suffixes to each name.
-- | Will be separated by spaces (unless the suffix starts with a comma)
-- | For example, given the following character:
-- |
-- |    Shane Cantlyn
-- |    $Jr. $, M.D.
-- |
-- | Yields the following names (in addition to the un-suffixed names)
-- |
-- |    Shane Jr., M.D.
-- |    Cantlyn Jr., M.D.
-- |    Shane Cantlyn Jr., M.D.
addSuffixes :: [String] -> [String] -> [String]
addSuffixes [] xs = xs
addSuffixes ss xs = xs ++ [x ++ suffixString ss | x <- xs]


-- | Combines a list of suffixes into one suffix, separating by space
-- | except when the suffix starts with a comma
suffixString :: [String] -> String
suffixString = concatMap (prep . strip) where
    prep [] = []
    prep trail@(',':_) = trail
    prep suffix = ' ':suffix


-- | Like 'suffixString', for prefixes
prefixString :: [String] -> String
prefixString = concatMap (prep . strip) where
    prep prefix = if null prefix then "" else prefix ++ " "


-- | Applies each prefix in turn to each string in turn
-- | If a character has multiple names, the prefixes will not be applied
-- | to the first (assumed informal) name.
-- |
-- | Only one prefix is applied at a time. So the following character:
-- |
-- |    Shane Cantlyn
-- |    ^Dr. ^Fr.
-- |
-- | Yields the following names (in addition to the un-prefixed ones)
-- |
-- |    Dr. Cantlyn, Fr. Cantlyn, Dr. Shane Cantlyn, Fr. Shane Cantlyn
applyPrefixes :: [String] -> [String] -> [String]
applyPrefixes _ [] = []
applyPrefixes ps [x] = x : [unwords [p, x] | p <- ps]
applyPrefixes ps (x:ys) = x : ys ++ [unwords [p, y] | p <- ps, y <- ys]


-- | Splits a name into the names that can be tagged, which include:
-- |
-- |    First name, Last name, First & Last Name, All names
splitIntoNames :: String -> [String]
splitIntoNames s = let str = strip s in case parseNames str of
    [] -> [str]
    [x] -> [x]
    [x, y] -> [x, y, unwords [x, y]]
    xs -> [x, z, unwords [x, z], unwords xs] where
        x = head xs
        z = last xs


-- | Of a character's tags, the first is the "full name" and the rest
-- | are pseudonyms. Therefore, the first of a character's tags
-- | will have the prefixes and suffixes applied, the rest will not.
charTags :: [String] -> [String] -> [String] -> [String]
charTags _ _ [] = []
charTags ps ss (n:ns) = expanded : ns where
    expanded = prefixString ps ++ n ++ suffixString ss


-- | Parse a name into names, respecting escaped whitespace
-- | Return the original name on failure
parseNames :: String -> [String]
parseNames str = case parse namesParser str str of
                    Left _ -> [str]
                    Right ns -> ns

namesParser :: GenParser Char st [String]
namesParser = many1 (whitespace *> nameParser <* whitespace)

nameParser :: GenParser Char st String
nameParser = except " \t\""
