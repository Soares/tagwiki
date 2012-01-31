module Note.Character where
import Control.Arrow
import Control.Applicative hiding ( (<|>) )
import Control.Name
import Data.List
import Data.String.Utils
import Data.Utils
import Note
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Pin ( fromName )
import qualified Control.Modifier as Mods
import qualified Data.Set as Set

newtype Character = Character { base :: Basic } deriving (Eq, Ord, Show)


instance Note Character where
    basic = base

-- | Adds prefixes and suffixes to tags.
-- | Character names will be split on spaces, that they may be referenced
-- | by either first or last or full names.
-- |
-- | If the character has multi-part names (i.e. "Van Halen"), escape the
-- | whitespace (i.e. "Van\ Halen").
-- |
-- | Only the first name is so split; all following names will not be touched.
    names c = alter (names $ basic c) where
        (ps, ss) = (prefixes &&& suffixes) c
        expand = addSuffixes ss . applyPrefixes ps . splitIntoNames
        alter (Name pri n:ns) = nub $ [Name pri x | x <- expand n] ++ ns
        alter [] = []

-- | The primary name is the prefixed and suffixed full name
    primaryName c = doHead "" expand $ map namePart $ names $ basic c where
        expand n = prefixString (prefixes c) ++ n ++ suffixString (suffixes c)

-- | Updates a character to add 'nicknames' to the qualifiers.
-- | Thus, if you have the following character:
-- |
-- |    Fredward Sharpe, Freddie
-- |
-- | He may be referenced as (for example):
-- |
-- |    |Fredward (Freddie) Sharpe|
    qualifiers c = qualifiers (basic c) `Set.union` extras where
        extras = Set.fromList $ map fromName $ drop 1 ns
        ns = names $ basic c


-- | Of a character's tags, the first is the "full name" and the rest
-- | are pseudonyms. Therefore, the first of a character's tags
-- | will have the prefixes and suffixes applied, the rest will not.
    tags c = Set.fromList $ expand $ map namePart $ names $ basic c where
        expanded n = prefixString (prefixes c) ++ n ++ suffixString (suffixes c)
        expand (n:ns) = expanded n : ns
        expand [] = []


parseCharacter :: Int -> GenParser Char st Character
parseCharacter i = Character <$> parseBasic i Mods.anyMod




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
